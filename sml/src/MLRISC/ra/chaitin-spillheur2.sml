(*
 * This module implements the Chaitin heuristic (but weighted by
 * priorities).  This version also takes into account of savings in
 * coalescing if a virtual is not spilled.  You should use this version
 * if your program uses direct style and makes use of calleesave registers.
 *)
functor ImprovedChaitinSpillHeur
    (val moveRatio : real 
      (* cost of move compared to load/store; should be <= 1.0 *)
    ) : RA_SPILL_HEURISTICS =
struct

   structure G = RAGraph

   open G

   exception NoCandidate

   (*
    * This dummy node is used during spilling.
    *)
   val dummyNode = NODE{pri=ref 0,adj=ref [],degree=ref 0,movecnt=ref 0,
                        color=ref PSEUDO, defs=ref [], uses=ref [],
                        movecost=ref 0,movelist=ref [], number= ~1}

   val mode = RACore.NO_OPTIMIZATION

   fun init() = ()

   (*
    * Potential spill phase.
    * Find a cheap node to spill according to Chaitin's heuristic.
    *)
    fun chooseSpillNode{graph, hasBeenSpilled, spillWkl} = 
    let fun chase(NODE{color=ref(ALIASED n),...}) = chase n
          | chase n = n
        val infiniteCost = 123456789.0
        val don'tUse     = 223456789.0

        (* Savings due to coalescing when a node is not spilled *)
        fun moveSavings(NODE{movecnt=ref 0, ...}) = 0.0
          | moveSavings(NODE{movelist, ...}) = 
            let fun loop([], savings) = 
                     real(foldr (fn ((_,a),b) => Int.max(a,b)) 0 savings)
                  | loop(MV{status=ref(WORKLIST | GEORGE_MOVE | BRIGGS_MOVE),
                            dst, src, cost, ...}::mvs, savings) = 
                    let fun add(c,[]) = [(c,cost)]
                          | add(c,(x as (c':int,s))::savings) =
                             if c = c' then (c',s+cost)::savings 
                             else x::add(c,savings)
                        val savings =
                           case chase dst of
                             NODE{color=ref(COLORED c), ...} => add(c,savings)
                           | _ =>
                           case chase src of
                             NODE{color=ref(COLORED c), ...} => add(c,savings)
                           | _ => savings
                   in  loop(mvs, savings) end
                 | loop(_::mvs, savings) = loop(mvs, savings)
            in loop(!movelist, []) end
  
        (* The spill worklist is maintained only lazily.  So we have
         * to prune away those nodes that are already removed from the
         * interference graph.  After pruning the spillWkl, 
         * it may be the case that there aren't anything to be 
         * spilled after all.
         *)

        (*
         * Choose node with the lowest cost and have the maximal degree
         *)
        fun chaitin([], best, lowestCost, spillWkl) = 
              (best, lowestCost, spillWkl)
          | chaitin(node::rest, best, lowestCost, spillWkl) = 
             (case chase node of
               node as NODE{number, pri, defs, uses,
                            degree=ref deg, color=ref PSEUDO,...} => 
               let fun cost() = 
                   let val moveSavings = moveRatio * moveSavings(node)
                   in  (real(!pri) + moveSavings) / real deg end
                   val cost = 
                      case (!defs, !uses) of
                        (_,[]) => (* defs but no use *)
                                  ~1.0 - real deg
                      | ([d],[u]) => (* defs after use; don't use *) 
                           if d = u+1 orelse d = u+2 then don'tUse else cost()
                      | _ => cost()
               in  if cost < lowestCost andalso not(hasBeenSpilled number)
                   then 
                     if lowestCost >= infiniteCost then (* not a real node *)
                        chaitin(rest, node, cost, spillWkl)
                     else  
                        chaitin(rest, node, cost, best::spillWkl)
                   else
                     chaitin(rest, best, lowestCost, node::spillWkl)
               end
             | _ => (* discard node *)
                chaitin(rest, best, lowestCost, spillWkl)
             )

        (* val _ = print("["^Int.toString(length spillWkl)^"]") *)

        val (potentialSpillNode, cost, newSpillWkl) = 
             chaitin(spillWkl, dummyNode, infiniteCost, [])
    in  case (potentialSpillNode, newSpillWkl) of
          (NODE{number= ~1, ...}, []) => {node=NONE, cost=cost, spillWkl=[]}
        | (NODE{number= ~1, ...}, _) => raise NoCandidate
        | (node, spillWkl) => {node=SOME node, cost=cost, spillWkl=spillWkl}
    end
end
