functor X86MemRegs(X86Instr:X86INSTR) = struct
  structure I = X86Instr

  fun error msg = MLRiscErrorMsg.impossible ("X86MemRegs." ^ msg)

  val sp = X86Cells.esp
  fun memReg regmap opnd = let
    (* see X86.prim.asm stack layout *)
    fun disp r = Int32.fromInt((regmap r-40) * 8 + X86Runtime.vFpStart)
  in
    case opnd
    of I.FDirect f => I.Displace{base=sp, disp=I.Immed(disp f), 
                                 mem=I.Region.stack}
     | _ => error "memReg"
  end
end
