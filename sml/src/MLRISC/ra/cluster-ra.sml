(*
 * This module provides services for the new RA when using the cluster
 * representation.  
 *
 * -- Allen
 *)

functor ClusterRA
   (structure Flowgraph : FLOWGRAPH
    structure Asm       : INSTRUCTION_EMITTER
    structure InsnProps : INSN_PROPERTIES
      sharing Flowgraph.I = Asm.I = InsnProps.I
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
   structure Spill  = RASpill(InsnProps)

   structure PrintCluster = PrintClusterFn
      (structure Flowgraph = F
       structure Asm = Asm
      )

   open G

   type flowgraph = F.cluster (* flowgraph is a cluster *)

   fun error msg = MLRiscErrorMsg.error("ClusterRA", msg)

   fun regmap(F.CLUSTER{regmap, ...}) = regmap

   fun dumpFlowgraph(msg, cluster, stream) =
       PrintCluster.printCluster stream 
          ("------------------ "^msg^" ------------------") cluster

   exception NotThere

   val Asm.S.STREAM{emit, ...} = Asm.makeStream []

   val dummyLabel = F.LABEL(Label.Label{id= ~1, addr=ref ~1, name=""})

   fun inc n = Word.toIntX(Word.fromInt n + 0w1)

   fun length l =
   let fun loop([],n)   = n
         | loop(_::l,n) = loop(l,inc n)
   in  loop(l,0) end

   fun services(cluster as F.CLUSTER{regmap, blocks, blkCounter, ...}) =
   let (* Create a graph based view of cluster *)
       val N            = !blkCounter
       val _            = if N >= 65536 then 
                              error "too many blocks" else ()

       (* blocks indexed by block id *)
       val blockTable   = A.array(N, dummyLabel)

       (* definitions indexed by block id+instruction id *)
       val defsTable    = A.array(N, A.array(0, [])) : node list A.array A.array
       val marked       = A.array(N, A.array(0, ~1))
    
       val stamp = ref 0

       (* Build the initial interference graph *)
       fun init [] = ()
         | init((b as F.BBLOCK{blknum, insns, ...})::blocks) =
           let val n = length(!insns)
               (* val m = Word.toIntX(Word.>>(Word.fromInt(n+8),0w3)) *) 
               val m = n+1
           in  UA.update(blockTable, blknum, b); 
               UA.update(marked, blknum, A.array(m, ~1));
               UA.update(defsTable, blknum, A.array(m, []));
               init blocks
           end
         | init(_::blocks) = init blocks
       val _ = init blocks

       (*
        * Construct program point 
        *)
       fun progPt(blknum, instrId) = 
           Word.toIntX(Word.<<(Word.fromInt blknum,0w14) + Word.fromInt instrId)
       fun blockNum pt = Word.toIntX(Word.>>(Word.fromInt pt,0w14))
       fun instrNum pt = Word.toIntX(Word.andb(Word.fromInt pt,0w16383))

       fun freq(pt) = 
           let val F.BBLOCK{freq, ...} = A.sub(blockTable, blockNum pt)
           in  !freq end

       (*
        * Remove pseudo use 
        *)
       fun rmPseudoUses [] = ()
         | rmPseudoUses(NODE{uses,...}::ns) = (uses := []; rmPseudoUses ns)

       (*
        * Perform incremental liveness analysis on register v 
        *)
       fun liveness(v, v' as NODE{uses, ...}, addEdge) = 
       let val st = !stamp
           val _  = stamp := st + 1
           fun foreachUseSite [] = ()
             | foreachUseSite(u::uses) =   
               let val b = blockNum u
                   val i = instrNum u
                   val block = UA.sub(blockTable, b)
               in  if i = 0 then 
                       liveOutAtBlock(block) (* live out *)
                   else 
                       liveInAtStmt(block, 
                                    UA.sub(defsTable, b),
                                    UA.sub(marked, b), i);
                   foreachUseSite uses
               end

           and visitPred block =
               let fun foreachPred([]) = ()
                     | foreachPred((b, _)::pred) =
                        (liveOutAtBlock b; foreachPred pred)
                   val F.BBLOCK{pred, ...} = block
               in  foreachPred(!pred) end

           and liveInAtStmt(block, defs, marked, pos) =  
               if pos >= A.length defs then visitPred block
               else if UA.sub(marked, pos) = st then ()
               else (UA.update(marked, pos, st);
                     liveOutAtStmt(block, defs, marked, inc pos)
                    )

           and liveOutAtStmt(block, defs, marked, pos) = 
                  (* v is live out *)
               if pos < A.length defs then
               let fun foreachDef([], kill) = kill
                     | foreachDef((d as NODE{number=r, ...})::ds, kill) = 
                       if r = v then foreachDef(ds, true)
                       else (addEdge(d, v'); foreachDef(ds, kill)) 
                   val killed = foreachDef(UA.sub(defs, pos), false)
               in  if killed then ()
                   else liveInAtStmt(block, defs, marked, pos)
               end
               else visitPred block

           and liveOutAtBlock(block as F.BBLOCK{blknum, ...}) = 
               (* v is live out at the current block *)
               let val marked = UA.sub(marked, blknum)
               in  if UA.sub(marked, 0) = st then ()
                   else 
                    (UA.update(marked, 0, st);
                     liveOutAtStmt(block, UA.sub(defsTable, blknum),
                                   marked, 1)
                    )
               end
             | liveOutAtBlock _ = ()

       in  foreachUseSite (!uses)
       end 

       (*
        * Building the interference graph
        *) 
       fun buildIt (cellkind, regmap, G as GRAPH{nodes, dedicated, ...}) = 
       let val newNodes   = Core.newNodes G
           val addEdge    = Core.addEdge G
           val getnode    = Intmap.map nodes
           val insnDefUse = Props.defUse cellkind
           val getCell    = C.getCell cellkind

           fun isDedicated r =
              r < 0 orelse 
              r < A.length dedicated andalso UA.sub(dedicated, r) 

           fun chase(NODE{color=ref(ALIASED n), ...}) = chase n
             | chase n = n

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
                           let val tmp as NODE{uses,defs=ref [d],...} = 
                                    chase(getnode r)
                           in  uses := [d-1]; (dst, tmp::tmps) end
                       | (_, dst) => (dst, tmps)
                   fun moves([], [], mv) = mv
                     | moves(d::ds, s::ss, mv) =
                       if isDedicated d orelse isDedicated s 
                       then moves(ds, ss, mv)
                       else
                       let val dst as NODE{number=d, ...} = chase(getnode d)
                           val src as NODE{number=s, ...} = chase(getnode s)
                       in if d = s then moves(ds, ss, mv)
                          else moves(ds, ss, MV{dst=dst, src=src,
                                                status=ref WORKLIST,
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
                       in  scan(rest, inc pt, inc i, mv, tmps)  
                       end
                   val (pt, i, mv, tmps) = 
                       scan(!insns, progPt(blknum,1), 1, mv, tmps)
                   val _ = if pt >= progPt(blknum+1, 0) then 
                              error "mkNodes: too many instructions"
                           else ()
                   fun fill i = 
                       if i < A.length dtab then 
                          (UA.update(dtab, i, []); fill(inc i))
                       else ()
               in  fill i;
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
           fun mkEdges(v, v' as NODE{color=ref(PSEUDO | COLORED _), ...}) = 
                 liveness(v, v', addEdge) 
             | mkEdges _ = ()

           val (moves, tmps) = mkNodes(blocks, [], [])
       in  Intmap.app mkEdges nodes; 
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
       fun grow(b, n) =
       let (* val m = Word.toIntX(Word.>>(Word.fromInt(n+8),0w3)) *) 
           val m = n+1
       in  if A.length(A.sub(marked, b)) < m then
              UA.update(marked, b, A.array(m, ~1))
            else ();
           if A.length(A.sub(defsTable, b)) < m then
              UA.update(defsTable, b, A.array(m, []))
           else ()
       end 

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
       fun spill{copyInstr, spill, reload, graph as G.GRAPH{regmap,...}, 
                 cellkind, nodes=nodesToSpill} = 
       let
           val spillRewrite = Spill.spillRewrite
                              { graph=graph,
                                spill=spill,
                                reload=reload,
                                copyInstr=copyInstr,
                                cellkind=cellkind
                              }

           (* maps program point to registers to be spilled *)
           val spillSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

           (* maps program point to registers to be reloaded *)
           val reloadSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

           (* maps program point to registers to be killed *)
           val killSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

           (* set of basic blocks that are affected *)
           val affectedBlocks = Intmap.new(32, NotThere) : bool Intmap.intmap

           val addAffectedBlocks = Intmap.add affectedBlocks

           fun ins set = 
           let val add  = Intmap.add set
               val look = Intmap.mapWithDefault(set, [])
           in  fn (x, r) =>
               (add (x, r::look x);
                addAffectedBlocks (blockNum x, true)
               )
           end

           val insSpillSet  = ins spillSet
           val insReloadSet = ins reloadSet
           val insKillSet   = 
           let val add  = Intmap.add killSet
               val look = Intmap.mapWithDefault(killSet, [])
           in  fn (x, r) => add (x, r::look x) end

           fun get set = Intmap.mapWithDefault (set, [])
           val getSpillSet  = get spillSet
           val getReloadSet = get reloadSet
           val getKillSet   = get killSet

           val _ = app (fn G.NODE{number, defs=ref defs, uses=ref uses, ...} =>
                        (app (fn pt => insSpillSet (pt, number)) defs;
                         app (fn pt => insReloadSet (pt, number)) uses;
                         (* Definitions but no use! *) 
                         case uses of
                            [] => app (fn pt => insKillSet(pt, number)) defs
                          | _ => ()
                        )) nodesToSpill

           (* Rewrite a basic block *)
           fun rewrite(annotations, blknum, pt, [], newInstrs) = rev newInstrs
             | rewrite(annotations, blknum, pt, i::rest, newInstrs) = 
               let val spillRegs  = getSpillSet pt
                   val reloadRegs = getReloadSet pt
                   val killRegs   = getKillSet pt
               in  case (spillRegs, reloadRegs) of
                     ([], []) => 
                        rewrite(annotations, blknum, inc pt, rest, i::newInstrs)
                   | _ =>
                   (* do something with this instruction, dude! *)
                   let (* val _ = (print("Spilling: "^regs spillRegs); 
                                print(" Reloading: "^regs reloadRegs);
                                emit (C.lookup regmap) i) *)
                       val {code} = 
                        spillRewrite{instr=i, 
                                     spillRegs=spillRegs,
                                     reloadRegs=reloadRegs, 
                                     killRegs=killRegs,
                                     annotations=annotations}
                       (* val _ = (print("Code:"); 
                                app (emit (C.lookup regmap)) code) *)
                   in  rewrite(annotations, 
                               blknum, inc pt, rest, code @ newInstrs)
                   end
               end

           (* Rewrite all affected blocks *)
           fun rewriteAll (blknum, _) =
               let val F.BBLOCK{annotations, insns, ...} = 
                          A.sub(blockTable, blknum)
                   val instrs = 
                        rewrite(annotations,
                                blknum, progPt(blknum, 1), !insns, [])
               in  insns := instrs;
                   grow(blknum, length instrs)
               end

       in  Intmap.app rewriteAll affectedBlocks;
           app (fn G.NODE{color=ref(ALIASED_SPILL _), ...} => ()
                 | G.NODE{color, ...} => color := (SPILLED 0)
               ) nodesToSpill;
           rebuild(cellkind, graph)
       end

   in  {build=build, spill=spill, freq=freq}
   end

end
