functor X86MemRegs(X86Instr:X86INSTR) = struct
  structure I = X86Instr

  fun error msg = MLRiscErrorMsg.impossible ("X86MemRegs." ^ msg)

  val sp = X86Cells.esp
  fun memReg regmap opnd = let
    (* see X86.prim.asm stack layout *)
    fun fpDisp f = Int32.fromInt((regmap f-40) * 8 + X86Runtime.vFpStart)
    fun gpDisp r = Int32.fromInt(X86Runtime.vregStart +
                                 Word.toIntX(Word.<<(Word.fromInt(r-8),0w2)))

  in
    case opnd
    of I.FDirect f => I.Displace{base=sp, disp=I.Immed(fpDisp f), 
                                 mem=I.Region.stack}
     | I.MemReg r => I.Displace{base=sp, disp=I.Immed(gpDisp r),
                                mem=I.Region.stack}
     | _ => error "memReg"
  end
end
