(*
 * Generate a linear sequence of instructions
 *)
signature INSTR_GEN =
sig
   structure C : CELLS
   structure I : INSTRUCTIONS
   structure S : INSTRUCTION_STREAM

   sharing I.C = C 

   (* 
    * This function creates an instruction stream, which can be 
    * used to emit instruction into the instruction list.
    *)
   val newStream : I.instruction list ref -> 
                     (I.instruction, Annotations.annotations, 'a) S.stream

end
