functor AMD64MemRegs(AMD64Instr:AMD64INSTR) = struct
  structure I = AMD64Instr

  fun memReg{reg, base} = raise Fail "AMD64: memReg not yet implemented"
end
