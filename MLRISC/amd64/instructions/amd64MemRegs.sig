signature MEMORY_REGISTERS = sig
  structure I : AMD64INSTR
  val memReg : {reg:I.operand, base: CellsBasis.cell} -> I.ea
end
