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
   val EXECUTION_FREQ = A.new(SOME(fn r => "freq("^Int.toString r^")"))

    (* no effect at all; just allows you to insert comments *)
   val COMMENT = A.new(SOME(fn s => s))

   fun listify f =
   let fun g [] = ""
         | g [x] = f x
         | g (x::xs) = f x^" "^g xs
   in  g end

    (* control dependence use *)
   exception CTRLDEF of int 
   exception CTRLUSE of int
   local
      fun toString p = "p"^Int.toString p
   in
      val CTRL_USE = A.new'{create=CTRLUSE, 
                            get=fn CTRLUSE x => x | e => raise e, 
                            toString=toString}
      val CTRL_DEF = A.new'{create=CTRLDEF, 
                            get=fn CTRLDEF x => x | e => raise e, 
                            toString=toString}
   end

    (*
     * These annotations specifies definitions and uses                              * for a pseudo instruction.
     *)
   val REGINFO = A.new(SOME(fn _ => "REGINFO"))
                  : ((int -> int) * int -> string) A.property

   val NO_OPTIMIZATION = A.new(SOME(fn () => "NO_OPTIMIZATION"))
   val CALLGC = A.new(SOME(fn () => "CALLGC"))
   val GCSAFEPOINT = A.new(SOME(fn s => "GCSAFEPOINT: "^s))
   val BLOCK_NAMES = A.new(SOME(fn _ => "BLOCK_NAMES")) :
                       A.annotations A.property
   val EMPTY_BLOCK = A.new(SOME(fn () => "EMPTY_BLOCK"))

   exception MARKREG of int -> unit
   val MARK_REG = A.new'{toString=fn _ => "MARK_REG",
                         create=MARKREG,
                         get=fn MARKREG f => f | e => raise e
                        }
   val NO_BRANCH_CHAINING = A.new(SOME(fn () => "NO_BRANCH_CHAINING"))

end
