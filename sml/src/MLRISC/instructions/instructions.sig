(*
 *  This signature specifies the abstract view of an instruction.
 *)
signature INSTRUCTIONS =
sig
   structure C        : CELLS
   structure T        : MLTREE
   structure LabelExp : LABELEXP
   structure Constant : CONSTANT
   structure Region   : REGION
      sharing LabelExp.T = T
      sharing Constant = T.Constant
      sharing Region   = T.Region

   type operand         (* operand is abstract *)
   type ea              (* effective address is abstract *)
   type addressing_mode (* addressing mode *)
   type instruction     (* instruction is also abstract  *)
end
