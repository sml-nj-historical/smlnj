signature AMD64SHUFFLE = sig
  structure I : AMD64INSTR

  type t = {tmp:I.operand option, dst:CellsBasis.cell list, src:CellsBasis.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end
