(*
 * This module implements a Chow-Hennessy-style spill heuristic 
 *)
structure ChowHennessySpillHeur : RA_SPILL_HEURISTICS =
struct

   structure Core = RACore
   structure G    = RAGraph
   structure Heap = PriorityHeap

   open G

   exception NoCandidate

   val mode = Core.COMPUTE_SPAN
 
   val cache = ref NONE : (G.node * real) Heap.priority_queue option ref

   fun init() = cache := NONE

   (*
    * Potential spill phase.
    * Find a cheap node to spill according to Chow Hennessy's heuristic.
    *)
    fun chooseSpillNode{graph=G.GRAPH{span, ...}, 
                        hasBeenSpilled, spillWkl} = 
    let fun chase(NODE{color=ref(ALIASED n),...}) = chase n
          | chase n = n
        val lookupSpan = Intmap.mapWithDefault (span,0) 
  
        (* The spill worklist is maintained only lazily.  So we have
         * to prune away those nodes that are already removed from the
         * interference graph.  After pruning the spillWkl, 
         * it may be the case that there aren't anything to be 
         * spilled after all.
         *)

        fun chowHennessy([], L, pruned) = (L, pruned)
          | chowHennessy(node::rest, L, pruned) = 
             (case chase node of
               node as NODE{number, pri, defs, uses,
                            degree=ref deg, color=ref PSEUDO,...} => 
               if hasBeenSpilled number 
               then chowHennessy(rest, L, false)
               else
               let val span = real(lookupSpan number) 
                   val cost = real(!pri) / span
               in  case (!defs, !uses) of
                     (_,[]) => 
                       chowHennessy(rest, (node, ~1.0 - real deg)::L, false)
                   | ([d],[u]) => (* defs after use; don't use *) 
                       if d = u+1 orelse d = u+2 then 
                         chowHennessy(rest, L, false)
                       else 
                         chowHennessy(rest, (node, cost)::L, false)
                   | _ => chowHennessy(rest, (node, cost)::L, false)
               end
             | _ => (* discard node *)
                chowHennessy(rest, L, pruned)
             )

        fun chooseNode heap =
        let fun loop() = 
            let val (node,cost) = Heap.deleteMin heap
            in  case chase node of
                   node as NODE{color=ref PSEUDO, ...} =>
                      {node=SOME(node), cost=cost, spillWkl=spillWkl}
                |  _ => loop()    
            end
        in  loop()
        end handle _ => {node=NONE, cost=0.0, spillWkl=[]}

    in  case !cache of
          SOME heap => chooseNode heap
        | NONE => 
          let val (L, pruned) = chowHennessy(spillWkl, [], true)
          in  if pruned then (* done *) 
                 {node=NONE, cost=0.0, spillWkl=[]}
              else
                (case L of
                  [] => raise NoCandidate
                | _  => let fun rank((_,x), (_,y)) = Real.<(x, y)
                            val heap = Heap.fromList rank L
                        in  cache := SOME heap; 
                            chooseNode heap
                        end
                )
          end
    end
end
