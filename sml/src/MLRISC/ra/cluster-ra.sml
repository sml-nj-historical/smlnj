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
   (structure Flowgraph : FLOWGRAPH
    structure Asm       : INSTRUCTION_EMITTER
    structure InsnProps : INSN_PROPERTIES
      sharing Flowgraph.I = InsnProps.I = Asm.I
      sharing Asm.P = Flowgraph.P
   ) : RA_FLOWGRAPH =
struct
   structure F      = Flowgraph
   structure I      = F.I
   structure W      = F.W
   structure C      = I.C
   structure G      = RAGraph
   structure Props  = InsnProps
   structure Core   = RACore
   structure A      = Array 
   structure UA     = Unsafe.Array (* okay, I'm cheating a bit here *)
   structure Spill  = RASpill(structure InsnProps = InsnProps
                              structure Asm       = Asm
                             )

   structure PrintCluster = PrintCluster
      (structure Flowgraph = F
       structure Asm = Asm
      )

   open G

   fun isOn(flag,mask) = Word.andb(flag,mask) <> 0w0

   type flowgraph = F.cluster (* flowgraph is a cluster *)

   fun error msg = MLRiscErrorMsg.error("ClusterRA", msg)
  
   val mode = 0w0

   (*
   local
      val redundant_spills_on = MLRiscControl.getFlag "count-redundant-spills"
      val red_reloads   = MLRiscControl.getCounter "redundant-reloads"
   in fun countSpillsReloads nodesToSpill =
      if !redundant_spills_on then
      app (fn G.NODE{color=ref(G.ALIASED _), ...} => ()
            | G.NODE{number, defs=ref defs, uses=ref uses, pri, ...} => 
              let datatype defUse = DEF | USE
                  val defsUses = 
                        ListMergeSort.sort (fn ((a,_), (b,_)) => a < b)
                          (map (fn pt => (pt,DEF)) defs @
                           map (fn pt => (pt,USE)) uses)
                  fun scan((a,b)::(rest as (c,d)::_)) =
                      (if (a=c+1 orelse a=c+2) 
                          andalso blockNum a = blockNum c then
                          (case (b,d) of
                            (DEF, USE) => red_reloads := !red_reloads + 1
                          | (USE, USE) => red_reloads := !red_reloads + 1
                          | _ => ()
                          )
                       else ();
                       scan rest
                      )
                    | scan _ = ()
              in  scan defsUses
              end
          ) nodesToSpill
      else ()
   end
   *)
 
   fun regmap(F.CLUSTER{regmap, ...}) = regmap

   fun dumpFlowgraph(msg, cluster, stream) =
       PrintCluster.printCluster stream 
          ("------------------ "^msg^" ------------------") cluster

   exception NotThere

   (*
   val Asm.S.STREAM{emit, ...} = Asm.makeStream []
    *)

   val dummyLabel = F.LABEL(Label.Label{id= ~1, addr=ref ~1, name=""})

   fun x + y = Word.toIntX(Word.+(Word.fromInt x, Word.fromInt y))

   fun length l =
   let fun loop([],n)   = n
         | loop(_::l,n) = loop(l,n+1)
   in  loop(l,0) end

   fun services(cluster as F.CLUSTER{regmap, blocks, blkCounter, ...}) =
   let (* Create a graph based view of cluster *)
       val N            = !blkCounter

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
       val blockTable   = A.array(N, dummyLabel)

       fun fillBlockTable [] = ()
         | fillBlockTable((b as F.BBLOCK{blknum, ...})::blocks) =
             (UA.update(blockTable, blknum, b); fillBlockTable blocks)
         | fillBlockTable(_::blocks) = fillBlockTable blocks
       val _ = fillBlockTable blocks

       (*
        * Building the interference graph
        *) 
       fun buildIt (cellkind, regmap, 
                    G as GRAPH{nodes, dedicated, mode, span, copyTmps, ...}) =

       let (* definitions indexed by block id+instruction id *)
           val defsTable    = A.array(N, A.array(0, [] : node list))
           val marked       = A.array(N, ~1)
        
           val stamp = ref 0
    
           (* Allocate the arrays *)
           fun alloc [] = ()
             | alloc((b as F.BBLOCK{blknum, insns, ...})::blocks) =
               let val n = length(!insns)
                   val m = n+1
               in  UA.update(defsTable, blknum, A.array(m, []));
                   alloc blocks
               end
             | alloc(_::blocks) = alloc blocks
           val _ = alloc blocks
    
           (*
            * Remove pseudo use generated by copy temporaries
            *)
           fun rmPseudoUses [] = ()
             | rmPseudoUses(NODE{uses,...}::ns) = (uses := []; rmPseudoUses ns)
    
           (*
            * Mark the use site as definitions so that the traversal would
            * end properly.
            *)
           fun markUseSites(v,[]) = ()
             | markUseSites(v,u::uses) =
               let val b    = blockNum u
                   val i    = instrNum u
                   val defs = UA.sub(defsTable, b)
               in  UA.update(defs, i, v::UA.sub(defs, i)); 
                   markUseSites(v, uses)
               end
    
           (*
            *  Unmark fake def sites
            *)
           fun unmarkUseSites [] = ()
             | unmarkUseSites(u::uses) = 
               let val b    = blockNum u
                   val i    = instrNum u
                   val defs = UA.sub(defsTable, b)
                   val _::instrs = UA.sub(defs, i)
               in  UA.update(defs, i, instrs); 
                   unmarkUseSites uses
               end
    
           (*
            * Perform incremental liveness analysis on register v 
            *)
           fun liveness(v, v' as NODE{uses, ...}, addEdge) = 
           let val st = !stamp
               val _  = stamp := st + 1
               fun foreachUseSite([]) = ()
                 | foreachUseSite(u::uses) =   
                   let val b = blockNum u
                       val i = instrNum u
                       val block = UA.sub(blockTable, b)
                   in  if i = 0 then 
                           liveOutAtBlock(block) (* live out *)
                       else 
                           liveOutAtStmt(block, UA.sub(defsTable, b), i+1);
                       foreachUseSite(uses)
                   end
    
               and visitPred(block) =
                   let fun foreachPred([]) = ()
                         | foreachPred((b, _)::pred) =
                            (liveOutAtBlock(b); foreachPred(pred))
                   in  case block of
                         F.BBLOCK{pred, ...} => foreachPred(!pred) 
                       | _ => error "visitPred"
                   end
    
               and liveOutAtStmt(block, defs, pos) = 
                      (* v is live out *)
                   if pos < A.length defs then
                   let fun foreachDef([], true) = ()
                         | foreachDef([], false) = 
                             liveOutAtStmt(block, defs, pos+1)
                         | foreachDef((d as NODE{number=r, ...})::ds, kill) = 
                           if r = v then foreachDef(ds, true)
                           else if r < 256 andalso v < 256 
                                then foreachDef(ds, kill)
                           else (addEdge(d,v'); foreachDef(ds, kill))
                   in  foreachDef(UA.sub(defs, pos), false)
                   end
                   else visitPred(block)
    
               and liveOutAtBlock(block as F.BBLOCK{blknum, ...}) = 
                   (* v is live out at the current block *)
                   if UA.sub(marked, blknum) = st then ()
                   else 
                     (UA.update(marked, blknum, st);
                      liveOutAtStmt(block, UA.sub(defsTable, blknum), 1)
                     )
                 | liveOutAtBlock _ = ()
    
               val useSites = SortedList.uniq(!uses)
           in  markUseSites(v', useSites);
               foreachUseSite(useSites);
               unmarkUseSites useSites
           end 
    
           (*
            * Perform incremental liveness analysis on register v 
            * and compute the span
            *)
           fun liveness2(v, v' as NODE{uses, ...}, addEdge, setSpan) = 
           let val st = !stamp
               val _  = stamp := st + 1
               fun foreachUseSite([], span) = span
                 | foreachUseSite(u::uses, span) =   
                   let val b = blockNum u
                       val i = instrNum u
                   in  case UA.sub(blockTable, b) of
                         block as F.BBLOCK{freq, ...} => 
                         let val span =
                               if i = 0 then 
                                  liveOutAtBlock(block, span) (* live out *)
                              else 
                                  let val f = !freq
                                  in  liveOutAtStmt(block, UA.sub(defsTable, b),
                                                    i+1, f, span+f)
                                  end;
                         in  foreachUseSite(uses, span)
                         end
                       | _ => error "liveness2"
                   end
    
               and visitPred(block, span) =
                   let fun foreachPred([], span) = span
                         | foreachPred((b, _)::pred, span) =
                           let val span = liveOutAtBlock(b, span)
                           in  foreachPred(pred, span) end
                   in  case block of
                         F.BBLOCK{pred, ...} => foreachPred(!pred, span)
                       | _ => error "visitPred"
                   end
    
               and liveOutAtStmt(block, defs, pos, freq, span) = 
                      (* v is live out *)
                   if pos < A.length defs then
                   let fun foreachDef([], true) = span
                         | foreachDef([], false) = 
                              liveOutAtStmt(block, defs, pos+1, freq, span+freq)
                         | foreachDef((d as NODE{number=r, ...})::ds, kill) = 
                           if r = v then foreachDef(ds, true)
                           else (addEdge(d, v'); foreachDef(ds, kill)) 
                   in foreachDef(UA.sub(defs, pos), false)
                   end
                   else visitPred(block, span)
    
               and liveOutAtBlock(block as F.BBLOCK{blknum, freq, ...}, span) = 
                   (* v is live out at the current block *)
                   if UA.sub(marked, blknum) = st then span
                   else 
                      (UA.update(marked, blknum, st);
                       liveOutAtStmt(block, UA.sub(defsTable, blknum),
                                     1, !freq, span)
                      )
                 | liveOutAtBlock(_, span) = span
    
               val useSites = SortedList.uniq(!uses)
               val _        = markUseSites(v', useSites)
               val span     = foreachUseSite (useSites, 0)
               val _        = unmarkUseSites useSites
           in  setSpan(v, span)
           end 
    
           val newNodes   = Core.newNodes G
           val getnode    = Intmap.map nodes
           val insnDefUse = Props.defUse cellkind
           val getCell    = C.getCell cellkind

           fun isDedicated r =
              r < 0 orelse 
              r < A.length dedicated andalso UA.sub(dedicated, r) 

          (* Remove all dedicated or spilled registers from the list *)
           fun rmvDedicated regs =
           let fun loop([], rs') = rs'
                 | loop(r::rs, rs') = 
               let val r = regmap r 
               in  loop(rs, if isDedicated r then rs' else r::rs') end
           in  loop(regs, []) end
 
           (*
            * Create parallel move
            *)
           fun mkMoves(insn, dst, src, cost, mv, tmps) =
               if Props.moveInstr insn then
               let val (dst, tmps) = 
                       case (Props.moveTmpR insn, dst) of
                         (SOME r, _::dst) => 
                           (* Add a pseudo use for tmpR *)
                         let fun chase(NODE{color=ref(ALIASED n), ...}) = 
                                  chase n
                               | chase n = n
                         in  case chase(getnode r) of
                               tmp as NODE{uses,defs=ref [d],...} =>
                                  (uses := [d-1]; (dst, tmp::tmps)) 
                             | _ => error "mkMoves"
                         end
                       | (_, dst) => (dst, tmps)
                   fun moves([], [], mv) = mv
                     | moves(d::ds, s::ss, mv) =
                       if isDedicated d orelse isDedicated s 
                       then moves(ds, ss, mv)
                       else
                       let fun chases(NODE{color=ref(ALIASED src), ...}, dst) =
                                 chases(src, dst)
                             | chases(src, dst) = chases'(src, dst)
                           and chases'(src, NODE{color=ref(ALIASED dst), ...}) =
                                 chases'(src, dst)
                             | chases'(src, dst) = (src, dst)
                           val (src as NODE{number=s, ...},
                                dst as NODE{number=d, ...}) =
                                   chases(getnode s, getnode d)
                       in if d = s then moves(ds, ss, mv)
                          else moves(ds, ss, MV{dst=dst, src=src,
                                                status=ref WORKLIST,
                                                hicount=ref 0,
                                                (* kind=REG_TO_REG, *)
                                                cost=cost}::mv)
                       end
                     | moves _ = error "moves"
               in  (moves(dst, src, mv), tmps) end
               else (mv, tmps)

           (* Add the nodes first *)
           fun mkNodes([], mv, tmps) = (mv, tmps)
             | mkNodes(F.BBLOCK{blknum, insns, succ, freq=ref w, 
                                liveOut, ...}::blks, mv, tmps)= 
               let val dtab = A.sub(defsTable, blknum)
                   fun scan([], pt, i, mv, tmps) = (pt, i, mv, tmps)
                     | scan(insns as insn::rest, pt, i, mv, tmps) =
                       let val (d, u) = insnDefUse insn
                           val defs = rmvDedicated d
                           val uses = rmvDedicated u
                           val defs = newNodes{cost=w, pt=pt, 
                                               defs=defs, uses=uses}
                           val _    = UA.update(dtab, i, defs)
                           val (mv, tmps) = mkMoves(insn, d, u, w, mv, tmps)
                       in  scan(rest, pt+1, i+1, mv, tmps)  
                       end
                   val (pt, i, mv, tmps) = 
                       scan(!insns, progPt(blknum,1), 1, mv, tmps)
                   val _ = if pt >= progPt(blknum+1, 0) then 
                              error("mkNodes: too many instructions")
                           else ()
                   (* fun fill i = 
                       if i < A.length dtab then 
                          (UA.update(dtab, i, []); fill(i+1))
                       else () *)
               in  (* fill i; *)
                   (* If the block is escaping, then all liveout
                    * registers are considered used here.
                    *)
                   case !succ of
                      [(F.EXIT _, _)] =>
                      let val liveSet = rmvDedicated(getCell(!liveOut))
                      in  newNodes{cost=w, pt=progPt(blknum, 0),
                                   defs=[], uses=liveSet}; ()
                      end
                   | _ => ()
                   ;
                   mkNodes(blks, mv, tmps)
               end
             | mkNodes(_::blks, mv, tmps) = mkNodes(blks, mv, tmps)

          (* Add the edges *)

           val (moves, tmps) = mkNodes(blocks, [], [])
           val addEdge = Core.addEdge G
       in  Intmap.app 
             (if isOn(mode,Core.COMPUTE_SPAN) then
               let val spanMap = Intmap.new(Intmap.elems nodes, NotThere)
                   val setSpan = Intmap.add spanMap
                   val _       = span := SOME spanMap
               in  fn (v, v' as NODE{color=ref(PSEUDO | COLORED _), ...}) => 
                      liveness2(v, v', addEdge, setSpan) 
                    | (v, v' as NODE{color=ref(SPILLED c), ...}) => 
                      if c >= 0 then liveness2(v, v', addEdge, setSpan) else ()
                    | _ => ()
               end 
              else
               (fn (v, v' as NODE{color=ref(PSEUDO | COLORED _), ...}) => 
                    liveness(v, v', addEdge) 
                 | (v, v' as NODE{color=ref(SPILLED c), ...}) => 
                      if c >= 0 then liveness(v, v', addEdge) else ()
                 | _ => ()
               )
             ) nodes;
           if isOn(Core.SAVE_COPY_TEMPS, mode) then copyTmps := tmps else ();
           rmPseudoUses tmps;
           moves
       end

       (* 
        * Build the interference graph initially.
        *)
       fun build(G, cellkind) = buildIt(cellkind, C.lookup regmap, G)

       (*
        * Grow a table 
        *)
       (*
       fun grow(b, n) =
       let (* val m = Word.toIntX(Word.>>(Word.fromInt(n+8),0w3)) *) 
           val m = n+1
       in  (* if A.length(A.sub(marked, b)) < m then
              UA.update(marked, b, A.array(m, ~1))
           else (); *)
           if A.length(A.sub(defsTable, b)) < m then
              UA.update(defsTable, b, A.array(m, []))
           else ()
       end 
       *)

       (* 
        * Rebuild the interference graph;
        * We'll just do it from scratch for now.
        *)
       fun rebuild(cellkind, G) = 
           (Core.clearGraph G; Core.clearNodes G; 
            buildIt(cellkind, Core.regmap G, G))

       val regs = foldr(fn (r, "") => Int.toString r
                         | (r, l)  => Int.toString r^","^l) ""

       (*
        * Spill a set of nodes and rewrite the flowgraph 
        *)
       fun spill{copyInstr, spill, spillSrc, spillCopyTmp, 
                 reload, reloadDst, renameSrc, graph as G.GRAPH{regmap, ...}, 
                 cellkind, nodes=nodesToSpill} = 
       let
           (* maps program point to registers to be spilled *)
           val spillSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

           (* maps program point to registers to be reloaded *)
           val reloadSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

           (* maps program point to registers to be killed *)
           val killSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

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
           val affectedBlocks = Intmap.new(32, NotThere) : bool Intmap.intmap

           val addAffectedBlocks = Intmap.add affectedBlocks

           fun ins set = 
           let val add  = Intmap.add set
               val look = Intmap.mapWithDefault(set, [])
           in  fn r => fn x =>
               (add (x, r::look x);
                addAffectedBlocks (blockNum x, true)
               )
           end

           val insSpillSet  = ins spillSet
           val insReloadSet = ins reloadSet
           val insKillSet   = 
           let val add  = Intmap.add killSet
               val look = Intmap.mapWithDefault(killSet, [])
           in  fn r => fn x => add (x, r::look x) end

           (* Mark all spill/reload locations *)
           val markSpills = app 
              (fn G.NODE{color, number, defs=ref defs, uses=ref uses, ...} =>
               let fun spillIt(defs, uses) = 
                       (app (insSpillSet number) defs;
                        app (insReloadSet number) uses;
                        (* Definitions but no use! *) 
                        case uses of
                           [] => app (insKillSet number) defs
                         | _ => ()
                       )
               in  case !color of
                     G.SPILLED c => spillIt(defs, uses)
                   | G.PSEUDO => spillIt(defs, uses)
                   | _ => ()
               end
             ) 
           val _ = markSpills nodesToSpill

           (* Print all spill/reload locations *)
           (* val _ = countSpillsReloads nodesToSpill *) 
           (*
           val _ = 
              if !(MLRiscControl.getFlag "dump-spills") then
              app 
              (fn G.NODE{color=ref(G.ALIASED _), ...} => ()
                | G.NODE{number, defs=ref defs, uses=ref uses, pri, ...} => 
                  let fun pr pt = let val b = blockNum pt
                                      val p = instrNum pt
                                  in  Int.toString b^"."^Int.toString p end
                      fun prs pts = foldr (fn (pt,"") => pr pt
                                            | (pt,l) => pr pt^", "^l) "" pts
                  in   case 
                         (if List.exists 
                           (fn def => List.exists (fn use => use=def-1) uses)
                          defs then (print "DEF-USE "; true) else false,
                          if List.exists 
                          (fn use => List.exists (fn use2 => use=use2-1) uses)
                          uses then (print " USE-USE "; true) else false) of
                         (false,false) => ()
                       | _ =>
                          (print("Spilling ["^Int.toString(!pri)^
                                 "] r"^Int.toString number);
                           print(" defs="^prs(SortedList.uniq defs));
                           print(" uses="^prs(SortedList.uniq uses));
                           print "\n")
                  end
              ) nodesToSpill
              else ()
            *)

           (* Rewrite all affected blocks *)
           fun rewriteAll (blknum, _) =
               case A.sub(blockTable, blknum) of
                  F.BBLOCK{annotations, insns as ref instrs, ...} => 
                  let val instrs = 
                          spillRewrite{pt=progPt(blknum, length instrs),
                                       instrs=instrs,
                                       annotations=annotations}
                  in  insns := instrs
                      (* grow(blknum, length instrs) *)
                  end
               | _ => error "rewriteAll"

       in  Intmap.app rewriteAll affectedBlocks;
           let val spilledMarker = SPILLED ~2
           in  app (fn G.NODE{number, color as ref(SPILLED c), ...} => 
                        if number <> c then color := spilledMarker else ()
                     | G.NODE{color as ref PSEUDO, ...} => 
                        color := spilledMarker 
                     | _ => ()
                   ) nodesToSpill
           end;
           rebuild(cellkind, graph)
       end

   in  {build=build, spill=spill, 
        programPoint=fn{block,instr} => progPt(block,instr),
        blockNum=blockNum, 
        instrNum=instrNum
       }
   end

end
