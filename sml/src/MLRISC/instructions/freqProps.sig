(*
 * This is the abstract interface for extracting various kinds of
 * frequency information from the program.
 *)

signature FREQUENCY_PROPERTIES =
sig

   structure I : INSTRUCTIONS

      (* Branch probability in percentage *)
   val branchProb : I.instruction -> int

end
