(*
 * Control flow graph data structure used by the MLRISC IR.
 * All basic optimizations are based on this representation.
 *
 * -- Allen
 *)

signature CONTROL_FLOW_GRAPH =
sig

   structure I : INSTRUCTIONS
   structure B : BLOCK_NAMES
   structure P : PSEUDO_OPS
   structure C : CELLS
   structure W : FREQ
      sharing I.C = C
   
   type weight = W.freq

   datatype block_kind = 
       START          (* entry node *)
     | STOP           (* exit node *)
     | FUNCTION_ENTRY (* for SSA transformations *)
     | NORMAL         (* normal node *)
     | HYPERBLOCK     (* hyperblock *)

   and data = LABEL  of Label.label
            | PSEUDO of P.pseudo_op

   (*
    * NOTE: the instructions are listed in reverse order.
    * This choice is for a few reasons:
    * i)  Clusters represent instructions in reverse order, so keeping this
    *     the same avoid having to do conversions.
    * ii) This makes it easier to add instructions at the end of the block,
    *     which is more common than adding instructions to the front.
    * iii) This also makes it easier to manipulate the branch/jump instruction
    *      at the end of the block.
    *)
   
   and block = 
      BLOCK of
      {  id          : int,                        (* block id *)
         kind        : block_kind,                 (* block kind *)
         name        : B.name,                     (* block name *)
         freq        : weight ref,                 (* execution frequency *) 
         data        : data list ref,              (* data preceeding block *) 
         labels      : Label.label list ref,       (* labels on blocks *) 
         insns       : I.instruction list ref,     (* in rev order *)
         annotations : Annotations.annotations ref (* annotations *)
      }


   and edge_kind = ENTRY           (* entry edge *) 
                 | EXIT            (* exit edge *)
                 | JUMP            (* unconditional jump *)
                 | FALLSTHRU       (* falls through to next block *)  
                 | BRANCH of bool  (* branch *) 
                 | SWITCH of int   (* computed goto *)   
                 | SIDEEXIT of int (* the ith side exit in a hyperblock *)

   and edge_info = EDGE of { k : edge_kind,                  (* edge kind *)
                             w : weight ref,                 (* edge freq *)
                             a : Annotations.annotations ref (* annotations *)
                           }

   type edge = edge_info Graph.edge
   type node = block Graph.node

   datatype info = 
       INFO of { regmap      : C.regmap ref,
                 annotations : Annotations.annotations ref,
                 firstBlock  : int ref,
                 reorder     : bool ref
               }

   type cfg = (block,edge_info,info) Graph.graph

  (*========================================================================
   *
   *  Various kinds of annotations on basic blocks
   *
   *========================================================================*)
   exception LIVEOUT of C.cellset       (* escaping live out information *)
   exception CHANGED of unit -> unit
   exception CHANGEDONCE of unit -> unit

  (*========================================================================
   *
   *  Methods for manipulating basic blocks
   *
   *========================================================================*)
   val newBlock          : int * B.name * W.freq ref -> block (* empty *)
   val newStart          : int * W.freq ref -> block          (* start node *)
   val newStop           : int * W.freq ref -> block          (* stop node *)
   val newFunctionEntry  : int * W.freq ref -> block  (* fun entry node *)
   val copyBlock         : int * block -> block       (* copy a block *)
   val defineLabel       : block -> Label.label       (* define a label *)
   val emit              : C.regmap -> block -> unit  (* emit assembly *)
   val show_block        : C.regmap -> block -> string 

  (*========================================================================
   *
   *  Methods for manipulating CFG
   *
   *========================================================================*)
   val cfg      : info -> cfg      (* create a new cfg *) 
   val new      : C.regmap -> cfg  (* create a new cfg *)
   val subgraph : cfg -> cfg       (* mark as subgraph *)
   val init     : cfg -> unit      (* add start/stop nodes *)
   val changed  : cfg -> unit      (* mark cfg as changed *)  

   val regmap         : cfg -> C.regmap
   val setRegmap      : cfg * C.regmap -> unit
   val setAnnotations : cfg * Annotations.annotations -> unit
   val reglookup : cfg -> C.register -> C.register
   val liveOut   : block -> C.cellset
   val fallsThruFrom : cfg * Graph.node_id -> Graph.node_id option
   val fallsThruTo   : cfg * Graph.node_id -> Graph.node_id option
   val removeEdge    : cfg -> edge -> unit

  (*========================================================================
   *
   *  For viewing
   *
   *========================================================================*)
   val viewStyle      : cfg -> (block,edge_info,info) GraphLayout.style
   val viewLayout     : cfg -> GraphLayout.layout
   val headerText     : block -> string
   val footerText     : block -> string
   val subgraphLayout : { cfg : cfg, subgraph : cfg } -> GraphLayout.layout

  (*========================================================================
   *
   *  Miscellaneous stuff
   *
   *========================================================================*)
   val cdgEdge : edge_info -> bool (* for building a CDG *)

end

