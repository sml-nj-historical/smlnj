(*
 * These are some basic annotations understood by the MLRISC system.
 * The MLRISC client can create its own annotations and propagate them
 * to MLRISC.  Client-defined annotations are ignored by MLRISC. 
 *
 * -- Allen
 *)

signature BASIC_ANNOTATIONS =
sig

    (* 
     * The branch probability of conditional branches. 
     * The client can attach this with conditional branches.
     * This has no effect otherwise. 
     *
     * Currently, the annotation is recognized by the static branch prediction
     * module. 
     *)
   val BRANCH_PROB : int Annotations.property (* in percentage (0-100)*) 

    (* The execution frequency of a basic block 
     * You can attach this at a basic block.
     *)
   val EXECUTION_FREQ : real Annotations.property

    (* No effect at all; this just allows you to insert comments *)
   val COMMENT : string Annotations.property

    (* 
     * Control dependence definition and use.
     *
     * To use these, the client should generate
     * control dependence virtual registers via Cells.newCell Cells.CTRL
     * and attach these annotations to instructions and basic blocks.
     *
     * These annotations are currently recognized by the SSA optimization
     * modules.
     *)
   datatype ctrl_dep = CTRL_DEF of int | CTRL_USE of int
   val CTRL : ctrl_dep Annotations.property

    (*
     * These annotations specifies definitions and uses 
     * for a pseudo instruction.
     *)
   val DEFUSER  : (int list * int list) Annotations.property
   val DEFUSEF  : (int list * int list) Annotations.property
   val DEFUSECC : (int list * int list) Annotations.property

    (*
     * This annotation can be used specify a pretty printing function for
     * assemblers
     *)
   val REGINFO : (int -> string) Annotations.property

    (*
     * This annotation can be used to turn on pretty printing of cellsets
     * inside assemblers
     *)
   val SHOW_CELLSET : unit Annotations.property

    (*
     * Disable all optimizations in the cluster
     *)
   val NO_OPTIMIZATION : unit Annotations.property

    (*
     * Mark basic block that is used for calling the GC
     *)
   val CALLGC : unit Annotations.property

    (*
     * Insert block names
     *)
   val BLOCK_NAMES : Annotations.annotations Annotations.property

    (*
     * This annotation inserts an empty basic block
     *)
   val EMPTY_BLOCK : unit Annotations.property

end
