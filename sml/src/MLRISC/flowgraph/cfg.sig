(* cfg.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Control flow graph data structure used by the MLRISC IR.
 * All basic optimizations are based on this representation.
 *
 * -- Allen
 *)

signature CONTROL_FLOW_GRAPH =
sig

   structure P : PSEUDO_OPS 
   structure I : INSTRUCTIONS 

   structure W : FREQ
   
   type weight = W.freq

   datatype block_kind = 
       START          (* entry node *)
     | STOP           (* exit node *)
     | NORMAL         (* normal node *)

   (*
    * NOTE 1: the instructions are listed in reverse order.
    * This choice is for a few reasons:
    *
    * i)  Clusters represent instructions in reverse order, so keeping this
    *     the same avoid having to do conversions.
    *
    * ii) This makes it easier to add instructions at the end of the block,
    *     which is more common than adding instructions to the front.
    *
    * iii) This also makes it easier to manipulate the branch/jump instruction
    *      at the end of the block.
    *
    * NOTE 2: 
    *  Alignment always appear before labels in a block.
    *)
   
   and block = 
      BLOCK of
      {  id          : int,                        (* block id *)
         kind        : block_kind,                 (* block kind *)
         freq        : weight ref,                 (* execution frequency *) 
         labels      : Label.label list ref,       (* labels on blocks *) 
         insns       : I.instruction list ref,     (* in rev order *)
	 align	     : P.pseudo_op option ref,	   (* alignment only *)
         annotations : Annotations.annotations ref (* annotations *)
      }


  (* We have the following invariant on blocks and out-edge kinds:
   *
   *	If the last instruction of the block is an unconditional jump, then
   *	there is one out edge labeled with JUMP.
   *
   *	If the last instruction of the block is a conditional jump, then
   *	there are two out edges.  The one corresponding to the jump is
   *	labeled BRANCH(true) and the other is labeled BRANCH(false).
   *
   *    If the last instruction of the block is not a jump, then there is
   *	one out edge labeled with FALLSTHRU.
   *
   *	If the block ends with a switch, then the out edges are labeled with
   *	SWITCH.
   *
   *	If the block ends with a call that has been wrapped with a FLOW_TO,
   *	then there will be one FALLSTHRU out edges and one or more FLOWSTO
   *	out edges.
   *
   *	Control-flow to outside the CFG is represented by edges to the unique
   *	STOP node.  When such edges are to labels that are defined outside
   *	the CFG, then JUMP, BRANCH, or SWITCH edges are used (as appropriate).
   *	When such edges are to unkonwn places (e.g., traps, returns, and
   *	indirect jumps), then an EXIT edge is used.  There should never be
   *	a FALLSTHRU or ENTRY edge to the STOP node.
   *)
    and edge_kind	    (* edge kinds *)
      = ENTRY			(* edge from START node *) 
      | EXIT            	(* unlabeled edge to STOP node *)
      | JUMP			(* unconditional jump *)
      | FALLSTHRU		(* falls through to next block *)  
      | BRANCH of bool		(* branch *) 
      | SWITCH of int		(* computed goto *)
      | FLOWSTO			(* FLOW_TO edge *)
   
    and edge_info = EDGE of {
	k : edge_kind,                  (* edge kind *)
	w : weight ref,                 (* edge freq *)
	a : Annotations.annotations ref (* annotations *)
      }

   type edge = edge_info Graph.edge
   type node = block Graph.node

   datatype info = 
       INFO of { annotations : Annotations.annotations ref,
                 firstBlock  : int ref, (* id of first block *)
                 reorder     : bool ref, (* has the CFG been reordered? *)
		 data        : P.pseudo_op list ref (* reverse order of generation *)
               }

   type cfg = (block,edge_info,info) Graph.graph

  (*========================================================================
   *
   *  Various kinds of annotations on basic blocks
   *
   *========================================================================*)
   val LIVEOUT : I.C.cellset Annotations.property
                  (* escaping live out information *)
   val CHANGED : (string * (unit -> unit)) Annotations.property

  (*========================================================================
   *
   *  Methods for manipulating basic blocks
   *
   *========================================================================*)
   val newBlock          : int * W.freq ref -> block (* empty *)
   val newStart          : int * W.freq ref -> block          (* start node *)
   val newStop           : int * W.freq ref -> block          (* stop node *)
   val copyBlock         : int * block -> block       (* copy a block *)
   val defineLabel       : block -> Label.label       (* define a label *)
   val insns             : block -> I.instruction list ref
   val freq              : block -> W.freq ref
   val branchOf          : edge_info -> bool option

               (* emit assembly *)
   val emit       : Annotations.annotations -> block -> unit

  (*========================================================================
   *
   *  Methods for manipulating CFG
   *
   *========================================================================*)
   val cfg      : info -> cfg      (* create a new cfg *) 
   val new      : unit -> cfg      (* create a new cfg *)
   val subgraph : cfg -> cfg       (* mark as subgraph *)
   val init     : cfg -> unit      (* add start/stop nodes *)
   val changed  : cfg -> unit      (* mark cfg as changed *)  

   val annotations    : cfg -> Annotations.annotations ref
   val liveOut        : block -> I.C.cellset
   val fallsThruFrom  : cfg * Graph.node_id -> Graph.node_id option
   val fallsThruTo    : cfg * Graph.node_id -> Graph.node_id option
   val removeEdge     : cfg -> edge -> unit
   val setBranch      : cfg * Graph.node_id * bool -> I.instruction
   val edgeDir        : edge_info Graph.edge -> bool option

  (*========================================================================
   *
   *  For viewing
   *
   *========================================================================*)
(*****
   val viewStyle      : cfg -> (block,edge_info,info) GraphLayout.style
   val viewLayout     : cfg -> GraphLayout.layout
   val headerText     : block -> string
   val footerText     : block -> string
   val subgraphLayout : { cfg : cfg, subgraph : cfg } -> GraphLayout.layout
*****)

  (*========================================================================
   *
   *  Miscellaneous stuff
   *
   *========================================================================*)
   val cdgEdge : edge_info -> bool (* for building a CDG *)

  (*========================================================================
   *
   *  Methods for printing CFGs
   *
   *========================================================================*)
   val show_block : Annotations.annotations -> block -> string 
   val show_edge  : edge_info -> string 
   val dump : (TextIO.outstream * string * cfg) -> unit

end

