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


signature RA_GRAPH = 
sig

  (*
   * The following are the data structures used in the register allocator.
   *)

  structure BM : BITMATRIX

  exception Nodes

  datatype interferenceGraph = 
   GRAPH of { bitMatrix    : BM.bitMatrix,
              nodes        : node Intmap.intmap,
              regmap       : int Intmap.intmap,
              K            : int,
              firstPseudoR : int,
              getreg       : 
                 {pref:int list,stamp:int,proh:int Array.array} -> int,
              proh         : int Array.array,
              stamp        : int ref,
             (* Info to undo a spill when an optimistic spill has occurred *)
              spillFlag    : bool ref,
              undoInfo     : (node * moveStatus ref) list ref
            }

  and moveStatus = MOVE | COALESCED | CONSTRAINED | LOST | WORKLIST

  and move = 
    MV of {src : node,			(* source register of move *)
	   dst : node,			(* destination register of move *)
	   status : moveStatus ref	(* coalesced? *)
	  }

  and nodeStatus = REMOVED | PSEUDO | ALIASED of node | COLORED of int

  and node = 
    NODE of { number : int,		(* node number *)
	      movecnt: int ref,		(* #moves this node is involved in *)
	      movelist: move list ref,	(* moves associated with this node *)
	      degree : int ref,		(* current degree *)
	      color : nodeStatus ref,	(* status *)
	      adj : node list ref	(* adjacency list *)
            }
  (* 
   * The valid transitions for a node are:
   * PSEUDO -> REMOVED			% during simplify
   * PSEUDO -> ALIASED(n)		% during coalescing
   * REMOVED -> COLORED(r)		% assigning a color
   *
   *  ... all others are illegal.
   *)

   type 'a worklist = 'a list
   type nodelist    = node worklist
   type movelist    = move worklist

   type ('sim,'move,'freeze,'spill,'stack) lists = 
      {simplifyWkl: 'sim,   (* nodes that can be simplified *)
       moveWkl : 'move,	    (* moves to be considered for coalescing *)
       freezeWkl : 'freeze, (* all n, s.t. degree(n)<K and moveRelated(n) *)
       spillWkl : 'spill,   (* all n, s.t. degree(n)>=K  *)
       stack : 'stack 	    (* nodes removed from the graph *)
      }

   type worklists = (nodelist,movelist,nodelist,nodelist,nodelist) lists

   (* Create a new interference graph *)
   val newGraph : {nodes        : node Intmap.intmap,
                   regmap       : int Intmap.intmap,
                   numRegs      : int,
                   K            : int,
                   firstPseudoR : int,
                   getreg       : 
                      {pref:int list,stamp:int,proh:int Array.array} -> int
                  } -> interferenceGraph

end

