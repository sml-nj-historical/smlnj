signature MEMORY_REGISTERS = sig
  structure I : X86INSTR
  val memReg : I.operand -> I.ea
end
