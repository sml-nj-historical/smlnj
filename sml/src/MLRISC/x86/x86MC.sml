(* X86MC.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * IMPORTANT NOTE: 
 *   Integer registers are numbered from 0 - 31 (0-7 are physical)
 *   Floating point registers are numbered from 32-63 (32-39 are physical)
 *)
functor X86MCEmitter
  (structure Instr : X86INSTR
   structure Shuffle : X86SHUFFLE where I = Instr
   structure MemRegs : MEMORY_REGISTERS where I = Instr
   structure AsmEmitter : INSTRUCTION_EMITTER where I = Instr) : MC_EMIT = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure LE = I.LabelExp
  structure W32 = Word32
  structure W8 = Word8
  structure W = LargeWord

  val itow  = Word.fromInt
  val wtoi  = Word.toInt

  fun error msg = MLRiscErrorMsg.impossible ("X86MCEmitter." ^ msg)

  (*
   * Sanity check! 
   *)
  val fpoffset = 32
  val _ = if C.FPReg 0 = fpoffset then () else error "Floating point encoding"

  val eax = 0   val esp = 4   
  val ecx = 1   val ebp = 5
  val edx = 2   val esi = 6   
  val ebx = 3   val edi = 7

  fun const c = Int32.fromInt(Const.valueOf c)
  fun lexp le = Int32.fromInt(LE.valueOf le)

  val toWord8 = 
    Word8.fromLargeWord o LargeWord.fromLargeInt o Int32.toLarge
  val eBytes = Word8Vector.fromList 
  fun eByte i = eBytes [Word8.fromInt i]
  fun eLong i32 = let
    val w = (W.fromLargeInt o Int32.toLarge) i32
    fun shift cnt = Word8.fromLargeWord(W.>>(w, cnt))
  in [shift(0w0), shift(0w8), shift(0w16), shift(0w24)]
  end

  fun emitInstrs(instrs, regmap) = 
    Word8Vector.concat(map (fn I => emitInstr(I, regmap)) instrs)

  and emitInstr(instr, regmap) = let
    val error = 
        fn msg =>
           let val AsmEmitter.S.STREAM{emit,...} = AsmEmitter.makeStream []
           in  emit regmap instr; error msg end

(*    val rNum = Intmap.map regmap *)
    fun rNum r = let
      val r' = regmap r
    in if r' >=0 andalso r' <= 7 then r' 
       else error ("rNum: bad register " ^ Int.toString r ^ " --> " ^
                    Int.toString r')
    end 
    fun fNum r = if r < 64 then r else regmap r

    val memReg = MemRegs.memReg fNum

    datatype size = Zero | Bits8 | Bits32
    fun size i = 
      if i = 0 then Zero
      else if Int32.<(i, 128) andalso Int32.<=(~128, i) then Bits8 
      else Bits32

    fun immedOpnd(I.Immed(i32)) = i32
      | immedOpnd(I.ImmedLabel le) = lexp le
      | immedOpnd(I.LabelEA le) = lexp le
      | immedOpnd _ = error "immedOpnd"

    nonfix mod    
    fun modrm{mod, reg, rm} = Word8.fromInt(mod*64 + reg*8 + rm)
    fun sib{ss, index, base} = Word8.fromInt(ss*64 + index*8 + base)

    fun eImmedExt(opc, I.Direct r) = [modrm{mod=3, reg=opc, rm=rNum r}]
      | eImmedExt(opc, opn as I.MemReg _) = eImmedExt(opc, memReg opn)
      | eImmedExt(opc, I.Displace{base, disp, ...}) = let
          val base = rNum base                (* XXX rNum may be done twice *)
          val immed = immedOpnd disp
          fun displace(mod, eDisp) = 
            if base=esp then 
              modrm{mod=mod, reg=opc, rm=4}::sib{ss=0, index=4, base=esp}::eDisp immed
            else
              modrm{mod=mod, reg=opc, rm=base} :: eDisp immed
        in
          case size immed
          of Zero => 
              if base=esp then 
               [modrm{mod=0, reg=opc, rm=4}, sib{ss=0,index=4,base=esp}]
             else if base=ebp then
               [modrm{mod=1, reg=opc, rm=ebp}, 0w0]
             else 
               [modrm{mod=0, reg=opc, rm=base}]
           | Bits8 => displace(1, fn i => [toWord8 i])
           | Bits32 => displace(2, eLong)
          (*esac*)
        end
      | eImmedExt(opc, I.Indexed{base=NONE, index, scale, disp, ...}) = 
         (modrm{mod=0, reg=opc, rm=4} ::
          sib{base=5, ss=scale, index=rNum index} :: 
          eLong(immedOpnd disp))
      | eImmedExt(opc, I.Indexed{base=SOME b, index, scale, disp, ...}) = let
          val index = rNum index
          val base = rNum b
          val immed = immedOpnd disp
          fun indexed(mod, eDisp) = 
            modrm{mod=mod, reg=opc, rm=4} ::
              sib{ss=scale, index=index, base=base} :: eDisp immed
        in
          case size immed
          of Zero => 
             if base=ebp then 
               [modrm{mod=1, reg=opc, rm=4},
                  sib{ss=scale, index=index, base=5}, 0w0]
             else
               [modrm{mod=0, reg=opc, rm=4}, 
                  sib{ss=scale, index=index, base=base}]
           | Bits8 => indexed(1, fn i => [toWord8 i])
           | Bits32 => indexed(2, eLong)
          (*esac*)
        end
      | eImmedExt(opc, opnd as I.FDirect f) = eImmedExt(opc, memReg opnd)
      | eImmedExt(_, I.Immed _) = error "eImmedExt: Immed"
      | eImmedExt(_, I.ImmedLabel _) = error "eImmedExt: ImmedLabel"
      | eImmedExt(_, I.Relative _) = error "eImmedExt: Relative"
      | eImmedExt(_, I.LabelEA _) = error "eImmedExt: LabelEA"

    fun condCode cond =
        (case cond
          of I.EQ => 0w4      | I.NE => 0w5
           | I.LT => 0w12     | I.LE => 0w14
           | I.GT => 0w15     | I.GE => 0w13
           | I.A  => 0w7      | I.AE => 0w3
           | I.B  => 0w2      | I.BE => 0w6
           | I.C  => 0w2      | I.NC => 0w3
           | I.P  => 0wxa     | I.NP => 0wxb
           | I.O  => 0w0      | I.NO => 0w1
        (*esac*))

    (* arith: only 5 cases need be considered:
     *  dst,   src
     *  -----------
     *  EAX,   imm32
     *        r/m32, imm32
     *  r/m32, imm8
     *        r/m32, r32
     *  r32,   r/m32
     *)
    fun arith(opc1, opc2) = let
      fun f(I.ImmedLabel le, dst) = f(I.Immed(lexp le), dst)
        | f(I.LabelEA le, dst) = f(I.Immed(lexp le), dst)
        | f(I.Immed(i), dst) = 
          (case size i
            of Bits32 => 
               (case dst
                of I.Direct r =>
                    if rNum r = 0 (* eax *) then 
                      eBytes(W8.fromInt(8*opc2 + 5) :: eLong(i))
                    else 
                      eBytes(0wx81 :: (eImmedExt(opc2, dst) @ eLong(i)))
                 | _ => 
                      eBytes(0wx81 :: (eImmedExt(opc2, dst) @ eLong(i)))
               (*esac*))
             | _ =>
               (* 83 /digit ib *)
               eBytes(0wx83 :: (eImmedExt(opc2,dst) @ [toWord8 i]))
          (*esac*))
        | f(src, I.Direct r) =
             eBytes((opc1+0w3)::eImmedExt(rNum r, src))
        | f(I.Direct r, dst) =
             eBytes((opc1 + 0w1) :: eImmedExt(rNum r, dst))
        | f _ = error "arith.f"
    in f
    end

    (* test:  the following cases need be considered:
     *  lsrc,  rsrc
     *  -----------
     *  AL,    imm8  opc1 A8
     *  EAX,   imm32 opc1 A9
     *  r/m8,  imm8  opc2 F6/0 ib
     *        r/m32, imm32 opc2 F7/0 id
     *        r/m8,  r8    opc3 84/r
     *        r/m32, r32   opc3 85/r
     *)
    fun test(bits, I.ImmedLabel le, lsrc) = test(bits, I.Immed(lexp le), lsrc)
      | test(bits, I.LabelEA le, lsrc) = test(bits, I.Immed(lexp le), lsrc)
      | test(bits, I.Immed(i), lsrc) =
        let val encoding =
                case (lsrc, i >= 0 andalso i < 255) of 
                  (I.Direct r, false) => 
                      if rNum r = 0 (* eax *) then (0wxA9::eLong i) 
                      else 0wxF7::(eImmedExt(0, lsrc)@eLong i)
                | (_, false)  => 0wxF7::(eImmedExt(0, lsrc) @ eLong i)
                | (I.Direct r, true) =>  (* 8 bit *)
                   let val r = rNum r 
                   in  if r = 0 (* eax *) then [0wxA8, toWord8 i]
                       else if r < 4 then 
                          (* unfortunately, only CL, DL, BL can be encoded *)
                          0wxF6::(eImmedExt(0, lsrc) @ [toWord8 i])
                       else if bits = 8 then error "test.8" 
                       else 0wxF7::(eImmedExt(0, lsrc) @ eLong i)
                   end
                | (_, true) => 0wxF6::(eImmedExt(0, lsrc) @ [toWord8 i])
        in  eBytes encoding
        end
      | test(8, rsrc as I.Direct r, lsrc) =
         if rNum r < 4 then 
            eBytes(0wx84 :: eImmedExt(rNum r, lsrc))
         else error "test.8"
      | test(32, I.Direct r, lsrc) =
         eBytes(0wx85 :: eImmedExt(rNum r, lsrc))
      | test _ = error "test"

  in
    case instr
    of I.NOP => eByte 0x90
     | I.JMP(r as I.Direct _, _) => eBytes(0wxff :: eImmedExt(4, r))
     | I.JMP(d as I.Displace _, _) => eBytes(0wxff :: eImmedExt(4, d))
     | I.JMP(m as I.MemReg _, _) => eBytes(0wxff :: eImmedExt(4, m))
     | I.JMP(i as I.Indexed _, _) => eBytes(0wxff :: eImmedExt(4, i))
     | I.JMP(I.Relative i, _) => ((let
         fun shortJmp() = eBytes[0wxeb, Word8.fromInt(i-2)]
       in
        case size(Int32.fromInt (i-2))
        of Bits32 => eBytes(0wxe9 :: eLong(Int32.fromInt(i-5)))
         | _ => shortJmp()
        (*esac*)
       end
       ) handle e => (print "JMP\n"; raise e))
     | I.JCC{cond, opnd=I.Relative i} => 
       let val code = condCode cond
       in  case size (Int32.fromInt(i-2))
            of Bits32 => 
               eBytes(0wx0f :: Word8.+(0wx80,code) :: eLong(Int32.fromInt(i-6)))
             | _ => 
                eBytes[Word8.+(0wx70,code), Word8.fromInt(i-2)]
       end 
     | I.CALL(I.Relative _, _, _, _) => error "CALL: Not implemented"
     | I.CALL(opnd, _, _, _) => eBytes(0wxff :: eImmedExt(2, opnd))
     | I.RET NONE => eByte 0xc3
     (* integer *)
     | I.MOVE{mvOp=I.MOVL, src, dst} => 
       let fun mv(I.Immed(i), I.Direct r) =
                 eBytes(Word8.+(0wxb8, Word8.fromInt(rNum r))::eLong(i))
             | mv(I.Immed(i), _) = 
                 eBytes(0wxc7 :: (eImmedExt(0, dst) @ eLong(i)))
             | mv(I.ImmedLabel le,dst) = mv(I.Immed(lexp le),dst)
             | mv(I.LabelEA le,dst) = error "MOVL: LabelEA"
             | mv(src as I.MemReg _, dst) = mv(memReg src, dst)
             | mv(src, dst as I.MemReg _) = mv(src, memReg dst)  
             | mv(src,dst) = arith(0wx88, 0) (src, dst)
       in  mv(src,dst) end
     | I.MOVE{mvOp=I.MOVB, dst, src=I.Immed(i)} =>
       (case size i
         of Bits32 => error "MOVE: MOVB: imm8"
          | _ => eBytes(0wxc6 :: (eImmedExt(0, dst) @ [toWord8 i]))
       (*esac*))
     | I.MOVE{mvOp=I.MOVB, dst, src=I.Direct r} => 
         eBytes(0wx88 :: eImmedExt(rNum r, dst))
     | I.MOVE{mvOp=I.MOVB, dst=I.Direct r, src} => 
         eBytes(0wx8a :: eImmedExt(rNum r, src))
     | I.MOVE{mvOp=I.MOVB, ...} => error "MOVE: MOVB"
     | I.MOVE{mvOp=I.MOVZBL, src=I.Immed _, ...} => error "MOVE: MOVZBL"
     | I.MOVE{mvOp=I.MOVZBL, src, dst=I.Direct r} =>
         eBytes(0wx0f :: 0wxb6 :: eImmedExt(rNum r, src))
     | I.MOVE _ => error "MOVE"
     | I.LEA{r32, addr} => eBytes(0wx8d :: eImmedExt(rNum r32, addr))
     | I.CMPL{lsrc, rsrc} => arith(0wx38, 7) (rsrc, lsrc)
     | (I.CMPW _ | I.CMPB _) => error "CMP"
     | I.TESTL{lsrc, rsrc} => test(32, rsrc, lsrc)
     | I.TESTB{lsrc, rsrc} => test(8, rsrc, lsrc)
     | I.TESTW _ => error "TEST"
     | I.BINARY{binOp, src, dst} => let
         fun shift(code, src) = 
            (case src
             of I.Immed (1) => eBytes(0wxd1 :: eImmedExt(code, dst))
              | I.Immed (n) => 
                 eBytes(0wxc1 :: (eImmedExt(code, dst)@ [toWord8 n]))
              | I.Direct r => 
                 if rNum r <> ecx then  error "shift: Direct"
                 else eBytes(0wxd3 :: eImmedExt(code, dst))
              | I.MemReg _ => shift(code, memReg src)
              | _  => error "shift"
             (*esac*))
       in
         case binOp
          of I.ADDL => arith(0w0, 0) (src, dst)
           | I.SUBL => arith(0wx28, 5) (src, dst)
           | I.ANDL => arith(0wx20, 4) (src, dst)
           | I.ORL  => arith(0w8, 1) (src, dst)
           | I.XORL => arith(0wx30, 6) (src, dst)
           | I.SHLL => shift(4,src)
           | I.SARL => shift(7,src)
           | I.SHRL => shift(5,src)
          (*esac*)
       end
     | I.MULTDIV{multDivOp, src} => let
         val mulOp = 
             (case multDivOp of I.MULL => 4 | I.IDIVL => 7 | I.DIVL => 6)
       in eBytes(0wxf7 :: eImmedExt(mulOp, src))
       end
     | I.MUL3{dst, src1, src2} => let
         val dst = rNum dst
       in
         case src2 
         of NONE => 
            (case src1
             of I.Immed(i) =>
                 (case size i
                  of Bits32 => 
                      eBytes(0wx69::(eImmedExt(dst, I.Direct(dst)) @ eLong i))
                   | _ =>
                      eBytes(0wx6b::(eImmedExt(dst, I.Direct(dst)) @ [toWord8 i])) 
                  (*esac*))
              | _ => eBytes(0wx0f::0wxaf::(eImmedExt(dst, src1)))
            (*esac*))
          | SOME i => 
            (case src1 
             of I.Immed _ => error "mul3: Immed"
              | I.ImmedLabel _ => error "mul3: ImmedLabel"
              | _ => 
                (case size i
                 of Bits32 => eBytes(0wx69 :: (eImmedExt(dst, src1) @ eLong(i)))
                  | _ => eBytes(0wx6b :: (eImmedExt(dst, src1) @ [toWord8 i]))
                 (*esac*))
            (*esac*))
        (*esac*)
       end
     | I.UNARY{unOp, opnd} => 
       (case unOp
        of I.DECL => 
            (case opnd
             of I.Direct d => eByte(0x48 + rNum d)
              | _ => eBytes(0wxff :: eImmedExt(1, opnd))
             (*esac*))
         | I.INCL =>
            (case opnd
             of I.Direct d => eByte(0x40 + rNum d)
              | _ => eBytes(0wxff :: eImmedExt(0, opnd))
             (*esac*))
         | I.NEGL => eBytes(0wxf7 :: eImmedExt(3, opnd))
         | I.NOTL => eBytes(0wxf7 :: eImmedExt(2, opnd))
        (*esac*))
     | I.SET{cond,opnd} => 
         eBytes(0wx0f :: Word8.+(0wx90,condCode cond) :: eImmedExt(0, opnd))
     | I.PUSHL(I.Immed(i)) => 
       (case size i 
        of Bits32 => eBytes(0wx68 :: eLong(i))
         | _ => eBytes[0wx6a, toWord8 i]
        (*esac*))
     | I.PUSHL(I.Direct r) => eByte(0x50+rNum r)
     | I.PUSHL opnd => eBytes(0wxff :: eImmedExt(6, opnd))
     | I.POP(I.Direct r) => eByte(0x58+rNum r)
     | I.POP(opnd) => eBytes(0wx8f :: eImmedExt(0, opnd))
     | I.CDQ => eByte(0x99)
     | I.INTO => eByte(0xce)

     | I.COPY{dst, src, tmp, ...} => let
        val instrs' = 
          Shuffle.shuffle
            {regmap=regmap, tmp=tmp, dst=dst, src=src}
       in emitInstrs(instrs', regmap)
       end

     | I.FCOPY{dst, src, tmp, ...} => let
        val instrs' = 
          Shuffle.shufflefp
            {regmap=regmap, tmp=tmp, dst=dst, src=src}
       in emitInstrs(instrs', regmap)
       end

     (* floating *)
     | I.FBINARY{binOp, src=I.ST 32, dst=I.ST 33} => 
       (case binOp
        of I.FADDP => eBytes[0wxde, 0wxc1]
         | I.FMULP => eBytes[0wxde, 0wxc9]
         | I.FDIVP => eBytes[0wxde, 0wxf1]
         | I.FDIVRP=> eBytes[0wxde, 0wxf9]
         | I.FSUBP => eBytes[0wxde, 0wxe1]
         | I.FSUBRP=> eBytes[0wxde, 0wxe9]

         | I.FADDL  => eBytes[0wxdc, 0wxc1]
         | I.FMULL  => eBytes[0wxdc, 0wxc9]
         | I.FDIVL  => eBytes[0wxdc, 0wxf1]
         | I.FDIVRL => eBytes[0wxdc, 0wxf9]
         | I.FSUBL  => eBytes[0wxdc, 0wxe1]
         | I.FSUBRL => eBytes[0wxdc, 0wxe9]
       (*esac*))
     | I.FBINARY{binOp, src, dst=I.ST 32} => let
         val (opc, code) = 
           (case binOp of 
                I.FADDL  => (0wxdc, 0) 
              | I.FMULL  => (0wxdc, 1) 
              | I.FCOML  => (0wxdc, 2) 
              | I.FCOMPL => (0wxdc, 3) 
              | I.FSUBL  => (0wxdc, 4) 
              | I.FSUBRL => (0wxdc, 5) 
              | I.FDIVL  => (0wxdc, 6)
              | I.FDIVRL => (0wxdc, 7)
              | I.FADDS  => (0wxd8, 0) 
              | I.FMULS  => (0wxd8, 1) 
              | I.FCOMS  => (0wxd8, 2) 
              | I.FCOMPS => (0wxd8, 3) 
              | I.FSUBS  => (0wxd8, 4) 
              | I.FSUBRS => (0wxd8, 5) 
              | I.FDIVS  => (0wxd8, 6)
              | I.FDIVRS => (0wxd8, 7)
              | _ =>  error "FBINARY:pop:dst=%st"
           (*esac*))
         val src' =            
           (case src
            of I.FDirect f => memReg src
             | I.ST f => I.Direct(f-fpoffset)
             | mem => mem
            (*esac*))
       in eBytes(opc :: eImmedExt(code, src'))
       end
     | I.FIBINARY{binOp, src} => 
       let val (opc, code) =
             case binOp of
               I.FIADDL  => (0wxda, 0)
             | I.FIMULL  => (0wxda, 1)
             | I.FICOML  => (0wxda, 2)
             | I.FICOMPL => (0wxda, 3)
             | I.FISUBL  => (0wxda, 4)
             | I.FISUBRL => (0wxda, 5)
             | I.FIDIVL  => (0wxda, 6)
             | I.FIDIVRL => (0wxda, 7)
             | I.FIADDS  => (0wxde, 0)
             | I.FIMULS  => (0wxde, 1)
             | I.FICOMS  => (0wxde, 2)
             | I.FICOMPS => (0wxde, 3)
             | I.FISUBS  => (0wxde, 4)
             | I.FISUBRS => (0wxde, 5)
             | I.FIDIVS  => (0wxde, 6)
             | I.FIDIVRS => (0wxde, 7)
       in  eBytes(opc :: eImmedExt(code, src)) end
     | I.FUNARY unOp =>
        eBytes[0wxd9, 
	       case unOp 
                of I.FABS  => 0wxe1 
	         | I.FCHS  => 0wxe0 
		 | I.FSQRT => 0wxfa
		 | I.FSIN  => 0wxfe
		 | I.FCOS  => 0wxff
		 | I.FTAN  => 0wxf2
		 | I.FINCSTP => 0wxf7]
     | I.FXCH{opnd=33} => eBytes[0wxd9, 0wxc9]
     | I.FUCOMPP => eBytes[0wxda, 0wxe9]
     | I.FSTPL(f as I.FDirect _) => emitInstr(I.FSTPL(memReg f), regmap)
     | I.FSTPL opnd => eBytes(0wxdd :: eImmedExt(3, opnd))
     | I.FLD1 => eBytes[0wxd9,0wxe8]
     | I.FLDZ => eBytes[0wxd9,0wxee]
     | I.FLDL(f as I.FDirect _) => emitInstr(I.FLDL(memReg f), regmap)
     | I.FLDL opnd => eBytes(0wxdd :: eImmedExt(0, opnd))
     | I.FILD opnd => eBytes(0wxdf :: eImmedExt(0, opnd))
     | I.FILDL opnd => eBytes(0wxdb :: eImmedExt(0, opnd))
     | I.FILDLL opnd => eBytes(0wxdf :: eImmedExt(5, opnd))
     | I.FNSTSW => eBytes[0wxdf, 0wxe0]

     (* misc *)
     | I.SAHF => eByte(0x9e)
     | I.ANNOTATION{i,...} => emitInstr(i,regmap)
     | _ => error "emitInstr"
  end 
end
