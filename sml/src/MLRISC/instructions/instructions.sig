(*
 *  This signature specifies the abstract view of an instruction.
 *)
signature INSTRUCTIONS =
sig
   structure C        : CELLS
   structure Constant : CONSTANT
   structure LabelExp : LABELEXP
   structure Region   : REGION
      sharing Constant = LabelExp.Constant

   type operand         (* operand is abstract *)
   type ea              (* effective address is abstract *)
   type addressing_mode (* addressing mode *)
   type instruction     (* instruction is also abstract  *)
end
