(*
 * This is the abstract interface for all instruction emitters, i.e.
 * assemblers and machine code emitters.
 *
 * -- Allen
 *)

signature INSTRUCTION_EMITTER =
sig

   structure I : INSTRUCTIONS
   structure C : CELLS
   structure S : INSTRUCTION_STREAM
   structure P : PSEUDO_OPS
      sharing I.C = C  
      sharing S.P = P

   val makeStream : unit -> (I.instruction,'a,'b) S.stream

end
