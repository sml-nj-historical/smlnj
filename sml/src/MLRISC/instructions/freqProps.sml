(*
 * Generic module for extracting the frequency information.
 *)

functor FreqProps(Props : INSN_PROPERTIES) : FREQUENCY_PROPERTIES =
struct

   structure I = Props.I

      (* Branch probability in percentage *)
   fun branchProb instr =
   let fun f(BasicAnnotations.BRANCH_PROB b::_) = b     
         | f(_::a) = f a
         | f [] = 50
   in  f(Props.getAnnotations instr) end

end
