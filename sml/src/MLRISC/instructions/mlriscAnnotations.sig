(*
 * These are some basic annotations understood by the MLRISC system.
 * The MLRISC client can create its own annotations and propagate them
 * to MLRISC.  Client-defined annotations are ignored by MLRISC. 
 *
 * -- Allen
 *)

signature MLRISC_ANNOTATIONS =
sig

    structure C : CELLS_BASIS = CellsBasis

    (* 
     * The branch probability of conditional branches. 
     * The client can attach this with conditional branches.
     * This has no effect otherwise. 
     *
     * Currently, the annotation is recognized by the static branch prediction
     * module. 
     *)
   exception BRANCHPROB of int
   val BRANCH_PROB : int Annotations.property (* in percentage (0-100)*) 

    (* The execution frequency of a basic block 
     * You can attach this at a basic block.
     *)
   exception EXECUTIONFREQ of int
   val EXECUTION_FREQ : int Annotations.property

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
   exception CTRLDEF of C.cell
   exception CTRLUSE of C.cell
   val CTRL_DEF : C.cell Annotations.property
   val CTRL_USE : C.cell Annotations.property

    (*
     * This annotation can be used specify a pretty printing function for
     * assemblers
     *)
   val REGINFO : (C.cell -> string) Annotations.property

    (*
     * Disable all optimizations in the cluster
     *)
   val NO_OPTIMIZATION : unit Annotations.property

    (*
     * Mark basic block that is used for calling the GC
     *)
   val CALLGC : unit Annotations.property
   val GCSAFEPOINT : string Annotations.property

    (*
     * Insert block names
     *)
   exception BLOCKNAMES of Annotations.annotations
   val BLOCK_NAMES : Annotations.annotations Annotations.property

    (*
     * This annotation inserts an empty basic block
     *)
   exception EMPTYBLOCK 
   val EMPTY_BLOCK : unit Annotations.property

    (* 
     * Enter information for a register.
     *)
   exception MARKREG of C.cell -> unit
   val MARK_REG : (C.cell -> unit) Annotations.property

    (*
     * Disable branch chaining optimization on a jump
     *)
   val NO_BRANCH_CHAINING : unit Annotations.property

end
