(*
 *  This signature specifies the abstract view of an instruction.
 *)
signature INSTRUCTIONS =
sig
   structure C        : CELLS

   type operand         (* operand is abstract *)
   type ea              (* effective address is abstract *)
   type addressing_mode (* addressing mode *)
   type instruction     (* instruction is also abstract  *)
end
