(*
 * Generic module for extracting the frequency information.
 *)

functor FreqProps(Props : INSN_PROPERTIES) : FREQUENCY_PROPERTIES =
struct

   structure I = Props.I

      (* Branch probability in percentage *)
   fun branchProb instr =
      case #get BasicAnnotations.BRANCH_PROB (#2(Props.getAnnotations instr)) of
        SOME b => b
      | NONE => 50

end
