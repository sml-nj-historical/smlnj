(*
 * This is the new register allocator based on
 * the 'iterated register coalescing' scheme described 
 * in POPL'96, and TOPLAS v18 #3, pp 325-353. 
 *
 * Now with numerous extensions:
 *
 *   0. Dead copy elimination (optional)
 *   1. Priority based coalescing
 *   2. Priority based freezing
 *   3. Priority based spilling
 *   4. Biased selection (optional)
 *   5. Spill Coalescing (optional)
 *   6. Spill Propagation (optional)
 *   7. Spill Coloring (optional)
 *
 * For details, please see the paper from
 *
 *    http://cm.bell-labs.com/cm/cs/what/smlnj/compiler-notes/index.html
 *
 * The basic structure of this register allocator is as follows:
 *   1.  RAGraph.  This module enscapsulates the interference graph 
 *       datatype (adjacency list + interference graph + node table)
 *       and contains nothing architecture specific.
 *   2.  RACore.  This module implements the main part of the iterated
 *       coalescing algorithm, with frequency enhancements.
 *   3.  RA_FLOWGRAPH.  This register allocator is parameterized
 *       with respect to this signature.  This basically abstracts out
 *       the representation of the program flowgraph, and provide
 *       a few services to the main allocator, such as building the 
 *       interference graph, rewriting the flowgraph after spilling,
 *       and rebuilding the interference graph after spilling.  
 *       This module is responsible for caching any information necessary 
 *       to make spilling fast.
 *   4.  This functor.  This functor drives the entire process.
 *
 * -- Allen Leung (leunga@cs.nyu.edu)
 *)

functor RegisterAllocator
   (SpillHeuristics : RA_SPILL_HEURISTICS) 
   (Flowgraph : RA_FLOWGRAPH) : RA =
struct

   structure F      = Flowgraph
   structure I      = F.I
   structure C      = I.C
   structure Core   = RACore
   structure G      = Core.G

   datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION 

   datatype optimization = DEAD_COPY_ELIM
                         | SPILL_PROPAGATION
                         | SPILL_COALESCING
                         | SPILL_COLORING
                         | BIASED_SELECTION

   type getreg = { pref  : C.cell list,
                   stamp : int,
                   proh  : int Array.array
                 } -> C.cell

   open G

   fun error msg = MLRiscErrorMsg.error("RegisterAllocator",msg)

   (*
    * Debugging flags + counters
    *)
   val cfg_before_ra     = MLRiscControl.getFlag "dump-cfg-before-ra"
   val cfg_after_ra      = MLRiscControl.getFlag "dump-cfg-after-ra"
   val cfg_after_spill   = MLRiscControl.getFlag "dump-cfg-after-spilling"
   val dump_graph        = MLRiscControl.getFlag "dump-interference-graph"
   val debug_spill       = MLRiscControl.getFlag "ra-debug-spilling"
   val ra_count          = MLRiscControl.getCounter "ra-count"
   val rebuild_count     = MLRiscControl.getCounter "ra-rebuild"
   val count_dead        = MLRiscControl.getFlag "ra-count-dead-code"
   val dead              = MLRiscControl.getCounter "ra-dead-code"
   val debug_stream      = MLRiscControl.debug_stream

   (*
    * Optimization flags
    *)
   val rematerialization = MLRiscControl.getFlag "ra-rematerialization"

   exception NodeTable

   (* This array is used for getreg.
    * We allocate it once. 
    *) 
   val proh = Array.array(C.firstPseudo, ~1)

   (*
    * Register allocator.  
    *    spillProh is a list of registers that are not candidates for spills.
    *)
   fun ra mode params flowgraph =
   let 
       (* Flowgraph methods *)
       val {build=buildMethod, spill=spillMethod, ...} = F.services flowgraph 

       (* Main function *)
       fun regalloc{getreg, K, dedicated, copyInstr,
                    spill, reload, spillProh, cellkind, optimizations} =
       if C.numCell cellkind () = 0 
       then ()
       else
       let fun getOpt([], dce, sp, sc, scolor, bs) = (dce, sp, sc, scolor, bs)
             | getOpt(DEAD_COPY_ELIM::opts, dce, sp, sc, scolor, bs) =
                 getOpt(opts, true, sp, sc, scolor, bs)
             | getOpt(SPILL_PROPAGATION::opts, dce, sp, sc, scolor, bs) =
                 getOpt(opts, dce, true, sc, scolor, bs)
             | getOpt(SPILL_COALESCING::opts, dce, sp, sc, scolor, bs) =
                 getOpt(opts, dce, sp, true, scolor, bs)
             | getOpt(SPILL_COLORING::opts, dce, sp, sc, scolor, bs) =
                 getOpt(opts, dce, sp, sc, true, bs)
             | getOpt(BIASED_SELECTION::opts, dce, sp, sc, scolor, bs) =
                 getOpt(opts, dce, sp, sc, scolor, true)

           val (deadCopyElim, spillProp, spillCoalescing, 
                spillColoring, biasedSelection) =
               getOpt(optimizations, false, false, false, false, false)

           (* extract the regmap and blocks from the flowgraph *)
           val regmap = F.regmap flowgraph (* the register map *)
    
           (* the nodes table *)
           val nodes  = Intmap.new(32,NodeTable) 
           (* create an empty interference graph *)
           val G      = G.newGraph{nodes=nodes, 
                                   K=K,
                                   dedicated=dedicated,
                                   numRegs=C.numCell cellkind (),
                                   maxRegs=C.maxCell,
                                   regmap=regmap,
                                   showReg=C.toString cellkind,
                                   getreg=getreg,
                                   getpair=fn _ => error "getpair",
                                   firstPseudoR=C.firstPseudo,
                                   proh=proh
                                  }
           val G.GRAPH{spilledRegs,...} = G
    
           val hasBeenSpilled = Intmap.mapWithDefault (spilledRegs,false)
    
           fun logGraph(header,G) = 
               if !dump_graph then 
                   (TextIO.output(!debug_stream,
                        "-------------"^header^"-----------\n");
                    Core.dumpGraph G (!debug_stream) 
                   )
               else ()
    
           (*
            * Build the interference graph 
            *) 
           fun buildGraph(G) = 
           let val moves = buildMethod(G,cellkind)
               val worklists = (Core.initWorkLists G) 
                                 {moves=moves, deadCopyElim=deadCopyElim}
           in  if !count_dead then
                  Intmap.app (fn (_,NODE{uses=ref [],...}) => dead := !dead + 1
                               | _ => ()) nodes
               else ();
               logGraph("build",G);
               worklists
           end
    
           (*
            * Potential spill phase
            *) 
           fun chooseVictim{spillWkl} =
           let fun dumpSpillCandidates(spillWkl) =
                   (print "Spill candidates:\n";
                    app (fn n => print(Core.show G n^" ")) spillWkl;
                    print "\n"
                   )
               val {node,cost,spillWkl} =
                   SpillHeuristics.chooseSpillNode
                       {hasBeenSpilled=hasBeenSpilled,
                        spillWkl=spillWkl}
                    handle SpillHeuristics.NoCandidate =>
                      (Core.dumpGraph G (!debug_stream);
                       dumpSpillCandidates(spillWkl);
                       error ("chooseVictim")
                      )
           in  if !debug_spill then
                  (case node of
                     NONE => ()
                   | SOME(best as NODE{defs,uses,...}) =>
                        print("Spilling node "^Core.show G best^
                              " cost="^Real.toString cost^
                              " defs="^Int.toString(length(!defs))^
                              " uses="^Int.toString(length(!uses))^"\n"
                             )
                  ) else ();
               {node=node,spillWkl=spillWkl}
           end 
              
           (*
            * Mark spill nodes
            *)
           fun markSpillNodes nodesToSpill =
           let val marker = SPILLED(~1)
               fun loop [] = ()
                 | loop(NODE{color, ...}::ns) = (color := marker; loop ns)
           in  loop nodesToSpill end
 
           (*
            * Actual spill phase.  
            *   Insert spill node and incrementally 
            *   update the interference graph. 
            *)
           fun actualSpills{spills} = 
           let val _ = if spillProp orelse spillCoalescing orelse
                          spillColoring then markSpillNodes spills
                       else ()
               val spills = if spillProp then 
                              Core.spillPropagation G spills else spills
               val _ = if spillCoalescing then 
                          Core.spillCoalescing G spills else ()
               val _ = if spillColoring then 
                          Core.spillColoring G spills else ()
               val {simplifyWkl,freezeWkl,moveWkl,spillWkl} =  
                    Core.initWorkLists G
                       {moves=spillMethod{graph=G, cellkind=cellkind,
                                          spill=spill, reload=reload,
                                          copyInstr=copyInstr, nodes=spills
                                         },
                        deadCopyElim=deadCopyElim
                       }
               val _ = if !cfg_after_spill then
                         F.dumpFlowgraph("after spilling",
                                         flowgraph,!debug_stream)
                       else ()
           in  logGraph("rebuild",G);
               rebuild_count := !rebuild_count + 1;
               (simplifyWkl, moveWkl, freezeWkl, spillWkl, [])
           end
           
           (*
            * Main loop of the algorithm
            *)
           fun main(G) =
           let 
                   
               (* Main loop *) 
               fun loop(simplifyWkl,moveWkl,freezeWkl,spillWkl,stack) =
               let val iteratedCoal = Core.iteratedCoalescing G
                   val potentialSpill = Core.potentialSpillNode G
                   (* simplify/coalesce/freeze/potential spill phases 
                    *    simplifyWkl -- non-move related nodes with low degree 
                    *    moveWkl     -- moves to be considered for coalescing
                    *    freezeWkl   -- move related nodes (with low degree)
                    *    spillWkl    -- potential spill nodes
                    *    stack       -- simplified nodes
                    *)
                   fun iterate(simplifyWkl,moveWkl,freezeWkl,spillWkl,stack) =
                   let (* perform iterated coalescing *)
                       val {stack} = iteratedCoal{simplifyWkl=simplifyWkl,
                                                  moveWkl=moveWkl,
                                                  freezeWkl=freezeWkl,
                                                  stack=stack}
                   in  case spillWkl of
                         [] => stack (* nothing to spill *)
                       |  _ => 
                         let val {node,spillWkl} = 
                                    chooseVictim{spillWkl=spillWkl}
                         in  case node of 
                               SOME node => (* spill node and continue *)
                               let val {moveWkl,freezeWkl,stack} = 
                                       potentialSpill{node=node,stack=stack}
                               in  iterate([],moveWkl,freezeWkl,spillWkl,stack)
                               end 
                             | NONE => stack (* nothing to spill *)
                         end
                   end

                   (* simplify the nodes *)
                   val stack = iterate
                          (simplifyWkl,moveWkl,freezeWkl,spillWkl,stack)
                   (* color the nodes *)
                   val {spills} = (Core.select G)
                                    {stack=stack, biased= biasedSelection}
               in  (* check for actual spills *)
                   case spills of
                     [] => ()
                   | spills => 
                     (case mode of
                       REGISTER_ALLOCATION => loop(actualSpills{spills=spills})
                     | COPY_PROPAGATION => ()
                     )
               end
    
               val {simplifyWkl, moveWkl, freezeWkl, spillWkl} = buildGraph G
    
           in  loop(simplifyWkl, moveWkl, freezeWkl, spillWkl, [])
           end
    
           fun initSpillProh(from,to) = 
           let val markAsSpilled = Intmap.add spilledRegs
               fun loop r = 
                   if r <= to then (markAsSpilled(r,true); loop(r+1)) else ()
           in  loop from end
    
       in  if !cfg_before_ra then
              F.dumpFlowgraph("before register allocation",
                              flowgraph,!debug_stream)
           else ();
           app initSpillProh spillProh;
           main(G); (* main loop *)
           (* update the regmap *)
           logGraph("done",G);
           case mode of
             REGISTER_ALLOCATION => Core.finishRA G
           | COPY_PROPAGATION => Core.finishCP G
           ;
           ra_count := !ra_count + 1;
           if !cfg_after_ra then 
              F.dumpFlowgraph("after register allocation",
                              flowgraph,!debug_stream) 
           else ()
       end

       fun regallocs [] = ()
         | regallocs(p::ps) = (regalloc p; regallocs ps)

   in  regallocs params;
       flowgraph
   end

end
