signature MEMORY_REGISTERS = sig
  structure I : X86INSTR
  val memReg : (int -> int) -> I.operand -> I.ea
end