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
   exception BRANCH_PROB of int (* in percentage (0-100)*) 

    (* The execution frequency of a basic block 
     * You can attach this at a basic block.
     *)
   exception EXECUTION_FREQ of real

    (* No effect at all; this just allows you to insert comments *)
   exception COMMENT of string 

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
   exception CTRL_DEF of int
   exception CTRL_USE of int

end
