(*
 *  This signature specifies the abstract view of an instruction.
 *)
signature INSTRUCTIONS =
sig
   structure C        : CELLS
   structure Constant : CONSTANT

   type operand     (* operand is abstract *)
   type ea          (* effective address is abstract *)
   type instruction (* instruction is also abstract  *)
end
