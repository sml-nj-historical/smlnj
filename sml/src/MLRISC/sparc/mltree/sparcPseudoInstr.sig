(*
 * sparcPseudoInstr.sig --- Sparc pseudo instructions 
 *)

signature SPARC_PSEUDO_INSTR = sig
   structure I : SPARCINSTR

   type format1 = 
       {r:I.C.cell, i:I.operand, d:I.C.cell} * 
       (I.operand -> I.C.cell) -> I.instruction list

   type format2 = 
       {i:I.operand, d:I.C.cell} * 
       (I.operand -> I.C.cell) -> I.instruction list
   (* 
    * Signed and unsigned multiplications.
    * For signed operations, trap on overflow and division by zero.
    * For unsigned operations, trap on division by zero.
    * These are all 32 bit operations 
    *)
   val umul : format1
   val smul : format1
   val udiv : format1
   val sdiv : format1

       (* convert integer into floating point *)
   val cvti2d : format2
   val cvti2s : format2
   val cvti2q : format2

       (* 32-bit overflow detection *)
   val overflowtrap32 : I.instruction list
 
       (* 64-bit overflow detection *)
   val overflowtrap64 : I.instruction list

end

