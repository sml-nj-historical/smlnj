(*
 * This module provides services for the new RA when using the cluster
 * representation.  
 * The algorithm is adapted from
 * Algorithm 19.17 from Appel, Modern Compiler Implementation in ML,
 * Calculation of live ranges in SSA form.  We don't directly use SSA 
 * here but the principles are the same.
 *
 * -- Allen
 *)

functor ClusterRA
   (structure Flowgraph : CONTROL_FLOW_GRAPH
    structure Asm       : INSTRUCTION_EMITTER
    structure InsnProps : INSN_PROPERTIES
    structure Spill : RA_SPILL 
      sharing Flowgraph.I = InsnProps.I = Asm.I = Spill.I 
      sharing Asm.P = Flowgraph.P
   ) : RA_FLOWGRAPH =
struct
   structure CFG    = Flowgraph
   structure I      = CFG.I
   structure W      = CFG.W
   structure G      = RAGraph
   structure Props  = InsnProps
   structure Core   = RACore
   structure A      = Array 
   structure UA     = Unsafe.Array (* okay, I'm cheating a bit here *)
   structure Spill  = Spill

   open G
   structure C      = I.C
   structure CB     = CellsBasis

   fun isOn(flag,mask) = Word.andb(flag,mask) <> 0w0

   val dump_size = MLRiscControl.getFlag "ra-dump-size"

   type flowgraph = CFG.cfg  (* flowgraph is a cluster *)

   fun error msg = MLRiscErrorMsg.error("ClusterRA", msg)

   val mode = 0w0

   fun uniqCells s = CB.SortedCells.return(CB.SortedCells.uniq s)

   fun chaseCell(c as CB.CELL{col=ref(CB.MACHINE r),...}) = (c,r)
     | chaseCell(CB.CELL{col=ref(CB.ALIASED c), ...}) = chaseCell c
     | chaseCell(c as CB.CELL{col=ref CB.SPILLED, ...}) = (c,~1)
     | chaseCell(c as CB.CELL{col=ref CB.PSEUDO, id, ...}) = (c,id)

   fun colorOf(CB.CELL{col=ref(CB.MACHINE r),...}) = r
     | colorOf(CB.CELL{col=ref(CB.ALIASED c), ...}) = colorOf c
     | colorOf(CB.CELL{col=ref CB.SPILLED, ...}) = ~1
     | colorOf(CB.CELL{col=ref CB.PSEUDO, id, ...}) = id

   fun chase(NODE{color=ref(ALIASED n), ...}) = chase n
     | chase n = n

   exception NotThere

   val Asm.S.STREAM{emit,...} = Asm.makeStream []

   fun dumpFlowgraph(txt, cfg as Graph.GRAPH graph, outstrm) = let
     fun say txt = TextIO.output(outstrm, txt)
     fun dump (nid, CFG.BLOCK{data, labels, insns, ...}) = let
       fun dumpData(CFG.LABEL lab) = say(Label.toString lab ^ "\n")
	 | dumpData(CFG.PSEUDO pOp) = say(CFG.P.toString pOp ^ "\n")
     in
       app dumpData (!data);
       app (fn lab => say(Label.toString lab ^ "\n")) (!labels);
       app emit (rev (!insns))
     end
   in app dump (#nodes graph ())
   end

   val dummyBlock =   CFG.newBlock(~1, ref 0)

   fun x + y = Word.toIntX(Word.+(Word.fromInt x, Word.fromInt y))

   fun services(cfg as Graph.GRAPH graph) = let
       val CFG.INFO{annotations=clAnns, ...} = #graph_info graph
       val blocks = #nodes graph ()
       fun maxBlockId ((id, CFG.BLOCK _)::rest, curr) = 
	   if id > curr then maxBlockId(rest, id) else maxBlockId(rest, curr)
	 | maxBlockId([], curr) = curr

       val N = maxBlockId(blocks, #capacity graph ())

       fun computeShift(0w0, max) = 0w31-max
         | computeShift(N, max) = computeShift(Word.>>(N, 0w1), Word.+(max,0w1))
       val shift = computeShift(Word.>>(Word.fromInt N, 0w15), 0w15)
       val mask  = Word.<<(0w1, shift) - 0w1

       (*
        * Construct program point 
        *)
       fun progPt(blknum, instrId) = 
           Word.toIntX(Word.+(Word.<<(Word.fromInt blknum, shift),
                                      Word.fromInt instrId))
       fun blockNum pt = Word.toIntX(Word.>>(Word.fromInt pt, shift))
       fun instrNum pt = Word.toIntX(Word.andb(Word.fromInt pt, mask))

           (* blocks indexed by block id *)
       val blockTable   = A.array(N, (#new_id graph (), dummyBlock))

       fun fillBlockTable [] = ()
         | fillBlockTable((b as (nid, _))::blocks) =
             (UA.update(blockTable, nid, b); fillBlockTable blocks)
       val _ = fillBlockTable blocks

       val EXIT = (case #exits graph () of [e] => e | _ => error "EXIT")

       (*
        * Building the interference graph
        *) 
       fun buildIt (cellkind,  
                    G as GRAPH{nodes, dedicated, mode, span, copyTmps, ...}) =

       let (* definitions indexed by block id+instruction id *)
           val defsTable    = A.array(N, A.array(0, [] : node list))
           val marked       = A.array(N, ~1)
           val addEdge      = Core.addEdge G

           (* copies indexed by source  
            * This table maps variable v to the program points where
            * v is a source of a copy.
            *)
           val copyTable    = IntHashTable.mkTable(N, NotThere) 
                : {dst:CB.cell,pt:int} list IntHashTable.hash_table
           val lookupCopy   = IntHashTable.find copyTable 
           val lookupCopy   = fn r => case lookupCopy r of SOME c => c 
                                                         | NONE => []
           val addCopy      = IntHashTable.insert copyTable
             
           val stamp = ref 0
    
           (* Allocate the arrays *)
           fun alloc [] = ()
	     | alloc((id, CFG.BLOCK{insns, ...})::blocks) = 
		(UA.update(defsTable, id, A.array(length(!insns)+1, []));
		 alloc blocks)
           val _ = alloc blocks
    
           (*
            * Remove pseudo use generated by copy temporaries
            *)
           fun rmPseudoUses [] = ()
             | rmPseudoUses(NODE{uses,...}::ns) = (uses := []; rmPseudoUses ns)
    
           (*
            * Initialize the definitions before computing the liveness for v.
            *)
           fun initialize(v, v', useSites) = let
               (* First we remove all definitions for all copies 
                * with v as source.
                * When we have a copy and while we are processing v
                *
                *      x <- v
                *
                *  x does not really interfere with v at this point,
                *  so we remove the definition of x temporarily.
                *)
               fun markCopies([], trail) = trail
                 | markCopies({pt, dst}::copies, trail) = 
                   let val b     = blockNum pt
                       val i     = instrNum pt
                       val defs  = UA.sub(defsTable, b)
                       val nodes = UA.sub(defs, i)
                       fun revAppend([], nodes) = nodes
                         | revAppend(n::ns, nodes) = revAppend(ns, n::nodes)
                       val dstColor = colorOf dst
                       fun removeDst([], nodes') = nodes'
                         | removeDst((d as NODE{number=r,...})::nodes, nodes')=
                           if r = dstColor then revAppend(nodes', nodes)
                           else removeDst(nodes, d::nodes')
                       val nodes' = removeDst(nodes, [])
                   in  UA.update(defs, i, nodes');
                       markCopies(copies, (defs, i, nodes)::trail)
                   end

               (*
                * Then we mark all use sites of v with a fake definition so that
                * the scanning will terminate correctly at these points.
                *) 
               fun markUseSites([], trail) = trail
                 | markUseSites(pt::pts, trail) = 
                   let val b     = blockNum pt
                       val i     = instrNum pt
                       val defs  = UA.sub(defsTable, b)
                       val nodes = UA.sub(defs, i)
                   in  UA.update(defs, i, v'::nodes);
                       markUseSites(pts, (defs, i, nodes)::trail)
                   end

               val copies = lookupCopy v
               val trail  = markCopies(copies, [])
               val trail  = markUseSites(useSites, trail)
           in  trail end
    
           fun cleanup [] = ()
             | cleanup ((defs, i, nodes)::trail) = 
                 (UA.update(defs, i, nodes); cleanup trail) 
           (*
            * Perform incremental liveness analysis on register v 
            * and compute the span
            *)
           fun liveness(v, v' as NODE{uses, ...}, cellV) = let
               val st = !stamp
               val _  = stamp := st + 1
               fun foreachUseSite([], span) = span
                 | foreachUseSite(u::uses, span) = let
                     val b = blockNum u
		     val i = instrNum u
		     val block as (_, CFG.BLOCK{freq, ...}) = UA.sub(blockTable, b)
		     val span =
		       if i = 0 then liveOutAtBlock(block, span) (* live out *)
		       else let 
			   val f = !freq
			   val defs = UA.sub(defsTable, b)
			 in  liveOutAtStmt(block, A.length defs, defs, i+1, f, span+f)
			 end
                   in foreachUseSite(uses, span)
                   end
    
               and visitPred((nid, _), span) =
                   let fun foreachPred([], span) = span
                         | foreachPred(nid::pred, span) = let
                              val span = liveOutAtBlock((nid, #node_info graph nid), span)
                           in  foreachPred(pred, span) 
			   end
                   in  
		     foreachPred(#pred graph nid, span)
                   end
    
               and liveOutAtStmt(block, nDefs, defs, pos, freq, span) = 
                      (* v is live out *)
                   if pos < nDefs then
                   let fun foreachDef([], true) = span
                         | foreachDef([], false) = 
                              liveOutAtStmt(block, nDefs, defs, 
                                            pos+1, freq, span+freq)
                         | foreachDef((d as NODE{number=r, ...})::ds, kill) = 
                           if r = v then foreachDef(ds, true)
                           else (addEdge(d, v'); foreachDef(ds, kill)) 
                   in foreachDef(UA.sub(defs, pos), false)
                   end
                   else visitPred(block, span)
    
               and liveOutAtBlock(block as (nid, CFG.BLOCK{freq, ...}), span) = 
                   (* v is live out at the current block *)
                   if UA.sub(marked, nid) = st then span
                   else let
		       val defs = UA.sub(defsTable, nid)
		     in
                       UA.update(marked, nid, st);
                       liveOutAtStmt(block, A.length defs, defs, 1, !freq, span)
                     end
   
               val useSites = SortedList.uniq(!uses) 
               val trail    = initialize(v, v', useSites)
               val span     = foreachUseSite (useSites, 0)
               val _        = cleanup trail
           in  
	     span
           end 

           val newNodes   = Core.newNodes G
           val getnode    = IntHashTable.lookup nodes
           val insnDefUse = Props.defUse cellkind
           val getCell    = C.getCellsByKind cellkind

           fun isDedicated r = dedicated r

          (* Remove all dedicated or spilled registers from the list *)
           fun rmvDedicated regs =
           let fun loop([], rs') = rs'
                 | loop(r::rs, rs') = 
                   let fun rmv(r as CB.CELL{col=ref(CB.PSEUDO), id, ...}) = 
			     if isDedicated(id) then loop(rs, rs') else loop(rs, r::rs')
                         | rmv(CB.CELL{col=ref(CB.ALIASED r), ...}) = rmv r
                         | rmv(r as CB.CELL{col=ref(CB.MACHINE col), ...}) = 
                             if isDedicated col then loop(rs, rs')
                             else loop(rs, r::rs')
                         | rmv(CB.CELL{col=ref(CB.SPILLED), ...}) = loop(rs,rs')
                   in  rmv r 
                   end
           in  loop(regs, []) end
 
           (*
            * Create parallel move
            *)
           fun mkMoves(insn, pt, dst, src, cost, mv, tmps) =
               if Props.moveInstr insn then
               let val (dst, tmps) = 
                       case (Props.moveTmpR insn, dst) of
                         (SOME r, _::dst) => 
                           (* Add a pseudo use for tmpR *)
                          (case chase(getnode(colorOf r)) of
                             tmp as NODE{uses,defs=ref [d],...} =>
                               (uses := [d-1]; (dst, tmp::tmps)) 
                          | _ => error "mkMoves"
                          )
                       | (_, dst) => (dst, tmps)
                   fun moves([], [], mv) = mv
                     | moves(d::ds, s::ss, mv) =
                       let val (d, cd) = chaseCell d
                           val (s, cs) = chaseCell s
                       in  if isDedicated cd orelse isDedicated cs
                           then moves(ds, ss, mv)
                           else if cd = cs then moves(ds, ss, mv)
                           else 
                             let val _ = 
                                  addCopy(cs, {dst=d, pt=pt}::lookupCopy cs);
                                 val dst = chase(getnode cd) 
                                 val src = chase(getnode cs) 
                             in  moves(ds, ss, MV{dst=dst, src=src,
                                                   status=ref WORKLIST,
                                                   hicount=ref 0,
                                                   (* kind=REG_TO_REG, *)
                                                   cost=cost}::mv
                                      ) 
                             end
                       end
                     | moves _ = error "moves"
               in  (moves(dst, src, mv), tmps) end
               else (mv, tmps)



           (* Add the nodes first *)
           fun mkNodes([], mv, tmps) = (mv, tmps)
	     | mkNodes((nid, blk)::blocks, mv, tmps) = let
	         val CFG.BLOCK{insns, freq=ref w, annotations, ...} = blk
	         val succ = #succ graph nid
		 val liveOut = CFG.liveOut blk
                 val dtab = A.sub(defsTable, nid)
		 fun scan([], pt, i, mv, tmps) = (pt, i, mv, tmps)
		   | scan(insn::rest, pt, i, mv, tmps) =
		     let val (d, u) = insnDefUse insn
			 val defs = rmvDedicated d
			 val uses = rmvDedicated u
			 val defs = newNodes{cost=w, pt=pt, 
					     defs=defs, uses=uses}
			 val _    = UA.update(dtab, i, defs)
			 val (mv, tmps) = 
			       mkMoves(insn, pt, d, u, w, mv, tmps)
		     in  scan(rest, pt+1, i+1, mv, tmps)  
		     end
		 val (pt, i, mv, tmps) = 
		   scan(!insns, progPt(nid,1), 1, mv, tmps)
		 val _ = 
		   if pt >= progPt(nid+1, 0) then 
		      error("mkNodes: too many instructions")
		   else ()
               in  
		 (* If the block is escaping, then all liveout
		  * registers are considered used here.
		  *)
		  case succ 
		   of [id] => 
		      if id = EXIT then let
                          val liveSet = rmvDedicated(
                                           uniqCells(getCell(liveOut)))
                        in  newNodes{cost=w, pt=progPt(nid, 0),
                                   defs=[], uses=liveSet}; ()
                        end
		      else ()
                    | _ => ()
                  (*esac*);
                  mkNodes(blocks, mv, tmps)
               end

          (* Add the edges *)

           val (moves, tmps) = mkNodes(blocks, [], [])
       in  
	   IntHashTable.appi
             (let val setSpan =
                  if isOn(mode,Core.COMPUTE_SPAN) then
                  let val spanMap = IntHashTable.mkTable
                                        (IntHashTable.numItems nodes, NotThere)
                      val setSpan = IntHashTable.insert spanMap
                      val _       = span := SOME spanMap
                  in  setSpan end
                  else fn _ => ()
              in  fn (v, v' as NODE{cell, color, ...}) =>
                  let fun computeLiveness() = 
                           setSpan(v, liveness(v, v', cell))
                  in  case !color of
                        PSEUDO => computeLiveness()
                      | COLORED _ => computeLiveness()
                      | MEMREG _ => computeLiveness()
                      | _ => ()
                  end
              end 
             ) nodes;
           if isOn(Core.SAVE_COPY_TEMPS, mode) then copyTmps := tmps else ();
           rmPseudoUses tmps;
           moves
       end (* buildIt *)

       (* 
        * Build the interference graph initially.
        *)
       fun build(G, cellkind) = let
         val moves = buildIt(cellkind, G)
	 val i2s = Int.toString
       in
	 if !dump_size then let 
	      val GRAPH{nodes, bitMatrix,...} = G
	      val insns = 
	        foldr (fn ((_,CFG.BLOCK{insns,...}),n) => length(!insns) + n) 0 blocks
            in 
	      TextIO.output
	         (!MLRiscControl.debug_stream,
		  "RA #blocks="^i2s N ^
	 	    " #insns="^i2s insns ^
                    " #nodes="^i2s(IntHashTable.numItems nodes) ^
                    " #edges="^i2s(Core.BM.size(!bitMatrix)) ^
                    " #moves="^i2s(length moves)^"\n")
            end
         else ();
         moves
       end

       (* 
        * Rebuild the interference graph;
        * We'll just do it from scratch for now.
        *)
       fun rebuild(cellkind, G) = 
           (Core.clearNodes G;
            buildIt(cellkind, G)
           )

       (*
        * Spill a set of nodes and rewrite the flowgraph 
        *)
       fun spill{copyInstr, spill, spillSrc, spillCopyTmp, 
                 reload, reloadDst, renameSrc, graph,
                 cellkind, nodes=nodesToSpill} = 
       let (* Remove the interference graph now *)
           val _ = Core.clearGraph graph

           (* maps program point to registers to be spilled *)
           val spillSet = IntHashTable.mkTable(32, NotThere)

           (* maps program point to registers to be reloaded *)
           val reloadSet = IntHashTable.mkTable(32, NotThere)

           (* maps program point to registers to be killed *)
           val killSet = IntHashTable.mkTable(32, NotThere) 

           val spillRewrite = Spill.spillRewrite
                              { graph=graph,
                                spill=spill,
                                spillSrc=spillSrc,
                                spillCopyTmp=spillCopyTmp,
                                reload=reload,
                                reloadDst=reloadDst,
                                renameSrc=renameSrc,
                                copyInstr=copyInstr,
                                cellkind=cellkind,
                                spillSet=spillSet,
                                reloadSet=reloadSet,
                                killSet=killSet
                              }

           (* set of basic blocks that are affected *)
           val affectedBlocks = IntHashTable.mkTable(32, NotThere)

           val addAffectedBlocks = IntHashTable.insert affectedBlocks

           fun ins set = let
               val add  = IntHashTable.insert set
               val look = IntHashTable.find set
               val look = fn r => case look r of SOME s => s | NONE => []
               fun enter(r, []) = ()
                 | enter(r, pt::pts) = 
                   (add (pt, r::look pt);
                    addAffectedBlocks (blockNum pt, true);
                    enter(r, pts)
                   )
           in  enter
           end

           val insSpillSet  = ins spillSet
           val insReloadSet = ins reloadSet
           val insKillSet   = 
	     let
               val add  = IntHashTable.insert killSet
               val look = IntHashTable.find killSet
               val look = fn r => case look r of SOME s => s | NONE => []
               fun enter(r, []) = ()
                 | enter(r, pt::pts) = (add(pt, r::look pt); enter(r, pts))
             in  enter 
             end

           (* Mark all spill/reload locations *)
           fun markSpills(G.NODE{color, number, cell, defs, uses, ...}) =
               let fun spillIt(defs, uses) = 
                       (insSpillSet(cell, defs);
                        insReloadSet(cell, uses);
                        (* Definitions but no use! *) 
                        case uses of
                           [] => insKillSet(cell, defs)
                         | _ => ()
                       )
		   val d = !defs
		   val u = !uses
               in  
		 case !color 
		 of G.SPILLED     => spillIt(d,u)
	 	  | G.SPILL_LOC _ => spillIt(d,u)
		  | G.MEMREG _    => spillIt(d,u)
                  | G.PSEUDO      => spillIt(d,u)
                  | _ => ()
               end
           val _ = app markSpills nodesToSpill

           (* Rewrite all affected blocks *)
           fun rewriteAll (blknum, _) = let
	     val (_, CFG.BLOCK{insns as ref instrs, annotations, ...}) = 
	       A.sub(blockTable, blknum)
	     val instrs = 
	       spillRewrite{pt=progPt(blknum, length instrs),
			    instrs=instrs, annotations=annotations}
           in
	     insns := instrs
           end


	   fun mark(G.NODE{color, ...}) = 
	     (case !color
	      of PSEUDO => color := SPILLED
	       | SPILLED => ()
	       | SPILL_LOC _ => ()
               | ALIASED _ => ()
	       | MEMREG _ => ()
	       | COLORED _ => error "mark: COLORED"
	       | REMOVED =>  error "mark: REMOVED"
             (*esac*))
       in 
	 IntHashTable.appi rewriteAll affectedBlocks;
	 app mark nodesToSpill;
	 rebuild(cellkind, graph)
       end (* spill *)
   in  
     { build       = build, 
       spill       = spill, 
       programPoint= fn{block,instr} => progPt(block,instr),
       blockNum    = blockNum, 
       instrNum    = instrNum
      }
  end
end

