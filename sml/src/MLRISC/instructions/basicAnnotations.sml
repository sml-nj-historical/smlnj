(*
 * These are some basic annotations understood by the MLRISC system
 *
 * -- Allen
 *)

structure BasicAnnotations : BASIC_ANNOTATIONS =
struct

    (* the branch probability of conditional branches *)
   exception BRANCH_PROB of int (* in percentage *) 

    (* the execution frequency of a basic block *)
   exception EXECUTION_FREQ of real

    (* no effect at all; just allows you to insert comments *)
   exception COMMENT of string

    (* control dependence definition and use *)
   exception CTRL_DEF of int
   exception CTRL_USE of int

   fun toString(BRANCH_PROB b)    = "branch("^Int.toString b^"%)"
     | toString(EXECUTION_FREQ r) = "freq("^Real.toString r^")"
     | toString(COMMENT s)        = s
     | toString(CTRL_DEF x)       = "ctrl-def "^Int.toString x
     | toString(CTRL_USE x)       = "ctrl-use "^Int.toString x
     | toString a                 = raise a

   val _ = Annotations.attachPrettyPrinter toString

end
