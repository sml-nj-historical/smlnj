(** Graph coloring register allocation.
 ** Implements the 'iterated register coalescing' scheme described 
 ** in POPL'96, and TOPLAS v18 #3, pp 325-353. 
 **
 ** RA CORE defines the core of the register allocator. 
 ** This basically means the enableMove, coalesce, simplify and freeze phases.
 ** These are separated out from the rest for more modularity 
 ** and customizability.
 ** 
 ** -- Allen
 **)


signature RA_CORE = 
sig

   structure G : RA_GRAPH

   (*
    * Basic functions
    *)
       (* add an edge *)
   val addEdge : G.interferenceGraph -> G.node * G.node -> unit
       (* debugging *)
   val dumpGraph : G.interferenceGraph -> unit 


   (* 
    * Core phases
    *)
   val makeWorkLists : G.interferenceGraph -> G.movelist  -> G.worklists
   val simplifyPhase : G.interferenceGraph -> G.worklists -> G.worklists
   val coalescePhase : G.interferenceGraph -> G.worklists -> G.worklists
   val freezePhase   : G.interferenceGraph -> G.worklists -> G.worklists
   val wklFromFrozen : int * G.node -> G.node list

      (* Return a list of spill nodes *)
   val optimisticSpilling : G.interferenceGraph -> G.worklists -> G.node list

      (* simplify/coalesce/freeze *)
   val simplifyCoalesceFreeze : G.interferenceGraph -> G.worklists -> G.worklists

      (* Update the regmap to be consistent with the node set, 
       * after register allocation or copy propagation 
       *)
   val finishRA : G.interferenceGraph -> unit 
   val finishCP : G.interferenceGraph -> unit 
end

