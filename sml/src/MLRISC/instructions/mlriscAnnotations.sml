(*
 * These are some basic annotations understood by the MLRISC system
 *
 * -- Allen
 *)

structure MLRiscAnnotations : MLRISC_ANNOTATIONS =
struct

   structure A = Annotations

    (* the branch probability of conditional branches *)
    (* in percentage *) 
   val BRANCH_PROB = A.new(SOME(fn b => "branch("^Int.toString b^"%)"))

    (* the execution frequency of a basic block *)
   val EXECUTION_FREQ = A.new(SOME(fn r => "freq("^Real.toString r^")"))

    (* no effect at all; just allows you to insert comments *)
   val COMMENT = A.new(SOME(fn s => s))

    (* control dependence definition and use *)
   datatype ctrl_dep = CTRL_DEF of int | CTRL_USE of int
   fun prCtrl (CTRL_DEF x) = "ctrl-def "^Int.toString x
     | prCtrl (CTRL_USE x) = "ctrl-use "^Int.toString x
   val CTRL = A.new(SOME prCtrl)

    (*
     * These annotations specifies definitions and uses                              * for a pseudo instruction.
     *)
   fun defUse(d,u) =
   let fun list l = 
           String.concat(foldr (fn (r,l) => Int.toString r::" "::l) [] l)
   in  "defs="^list d^" uses="^list u end

   val DEFUSER  = A.new(SOME(fn x => "reg "^defUse x))
   val DEFUSEF  = A.new(SOME(fn x => "freg "^defUse x))
   val DEFUSECC = A.new(SOME(fn x => "ccreg "^defUse x))

   val REGINFO = A.new(SOME(fn _ => "REGINFO")) : (int -> string) A.property

   val NO_OPTIMIZATION = A.newFlag("NO_OPTIMIZATION") 
   val CALLGC = A.newFlag("CALLGC") 
   val BLOCK_NAMES = A.new(SOME(fn _ => "BLOCK_NAMES")) :
                       A.annotations A.property
   val EMPTY_BLOCK = A.newFlag("EMPTY_BLOCK")
   val MARK_REG = A.new(SOME(fn _ => "MARK_REG")) : (int -> unit) A.property

end
