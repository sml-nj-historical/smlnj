signature MEMORY_REGISTERS = sig
  structure I : X86INSTR
  val memReg : {reg:I.operand, base: I.C.cell} -> I.ea
end
