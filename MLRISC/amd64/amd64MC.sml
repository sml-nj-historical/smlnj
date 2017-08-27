(* amd64MC.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor AMD64MCEmitter
  (structure Instr : AMD64INSTR
   structure Shuffle : AMD64SHUFFLE where I = Instr
   structure MLTreeEval : MLTREE_EVAL where T = Instr.T
   structure AsmEmitter : INSTRUCTION_EMITTER where I = Instr) : MC_EMIT = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure W32 = Word32
  structure W8 = Word8
  structure W = LargeWord
  structure CB = CellsBasis
  structure LE = MLTreeEval 

  val println = print o (fn s => s^"\n")
  val i2s = Int.toString
  val print_int = println o i2s

  val itow  = Word.fromInt
  val wtoi  = Word.toInt

  fun error msg = MLRiscErrorMsg.impossible ("AMD64MCEmitter." ^ msg)

  (*
   * Sanity check!
   *)

  val eax = 0   val esp = 4   
  val ecx = 1   val ebp = 5
  val edx = 2   val esi = 6   
  val ebx = 3   val edi = 7

  val opnd16Prefix = 0x66
		     
  fun const c = Int32.fromInt (Const.valueOf c)
  fun lexp le = Int32.fromInt (LE.valueOf le)
		
  val toWord8 = Word8.fromLargeWord o LargeWord.fromLargeInt o Int32.toLarge
  val eBytes = Word8Vector.fromList 
  fun eByte i = eBytes [W8.fromInt i]
  local 
      val toLWord = (W.fromLargeInt o Int32.toLarge) 
      fun shift (w,cnt) = W8.fromLargeWord(W.>>(w, cnt))
  in
  fun eShort i16 = 
      let val w = toLWord i16
      in [shift(w, 0w0), shift(w,0w8)]
      end
  fun eLong i32 = 
      let val w = toLWord i32
      in [shift(w, 0w0), shift(w,0w8), shift(w,0w16), shift(w,0w24)] end
  end
  fun eLongLong i64 =
      let
      val toLLWord = (Word64.fromLargeInt o Int64.toLarge) 
      val w = toLLWord i64
      fun shift64 (w,cnt) = let
          val shifted = Word64.>>(w, cnt)
          val masked = Word64.andb(0w255, shifted)
          in Word8.fromInt (Word64.toInt masked) end
      in [shift64(w, 0w0),
          shift64(w, 0w8),
          shift64(w, 0w16),
          shift64(w, 0w24),
          shift64(w, 0w32),
          shift64(w, 0w40),
          shift64(w, 0w48),
          shift64(w, 0w56)]
      end

  fun eLongLongCut i64 =
      let
      val toLLWord = (Word64.fromLargeInt o Int64.toLarge) 
      val w = toLLWord i64
      fun shift64 (w,cnt) = let
          val shifted = Word64.>>(w, cnt)
          val masked = Word64.andb(0w255, shifted)
          in Word8.fromInt (Word64.toInt masked) end
      in [shift64(w, 0w0), shift64(w, 0w8), shift64(w, 0w16), shift64(w, 0w24)]
      end
		
  fun emitInstrs instrs = Word8Vector.concat(map emitInstr instrs)
			   
  and emitAMD64Instr instr = 
      let val error = 
	   fn msg =>
              let val AsmEmitter.S.STREAM{emit,...} = AsmEmitter.makeStream []
              in  emit (I.INSTR instr); error msg end

	  datatype reg_or_opc = REG of int | OPC of int
	  val reg = REG and opcode = OPC
	  fun rMask r = r mod 8
	  fun getRO (REG r) = rMask r
	    | getRO (OPC oc) = oc
	  val rNum' = rMask o CB.physicalRegisterNum 
	  val rNum = CB.physicalRegisterNum 
	  val fNum = CB.physicalRegisterNum 
	  val isExtReg = (fn x => x > 7) o rNum
	  fun isExtReg' (REG r) = r > 7
	    | isExtReg' _ = false
		     
	  datatype size = Zero | Bits8 | Bits32
	  fun size i = 
	      if i = 0 then Zero
	      else if Int32.<(i, 128) andalso Int32.<=(~128, i) then Bits8 
	      else Bits32
		   
	  fun immedOpnd (I.Immed i32) = i32
	    | immedOpnd (I.ImmedLabel le) = lexp le
	    | immedOpnd (I.LabelEA le) = lexp le
	    | immedOpnd _ = error "immedOpnd"
			    
	  nonfix mod    
		
	  fun scale(n, m) = Word.toIntX(Word.<<(Word.fromInt n, Word.fromInt m))
	  fun modrm{mod, reg, rm} = W8.fromInt(scale(mod,6) + scale(reg,3) + rm)
	  fun sib{ss, index, base} = W8.fromInt(scale(ss,6) + scale(index,3) + base)
	  fun eREXRegs (r, x, b) =
	      let val rb1 = if r then 0wx4 else 0wx0
		  val rb2 = if x then rb1 + 0wx2 else rb1
		  val rb3 = if b then rb2 + 0wx1 else rb2
	      in 
		  if r orelse x orelse b then SOME rb3 else NONE
	      end (* rex *)
	  fun eREX rb = 0wx40 + rb
	  fun eREX64 rb = eREX rb + 0wx8
 
          fun eImmedExt (r', I.Direct (_, r)) = 
	      ( (isExtReg' r', false, isExtReg r), 
		[modrm{mod=3, reg=getRO r', rm=rNum' r}] )
            | eImmedExt (r', I.FDirect r) = 
	      ( (isExtReg' r', false, isExtReg r), 
		[modrm{mod=3, reg=getRO r', rm=rNum' r}] )
	    | eImmedExt (r', I.Displace{base=base', disp, ...}) =
	      let val base = rNum' base'
		  val immed = immedOpnd disp
		  val rex = (isExtReg' r', false, isExtReg base')
		  val r' = getRO r'
		  fun displace(mod, eDisp) = 
		      if base = esp then 
			  modrm{mod=mod, reg=r', rm=4}::
			  sib{ss=0, index=4, base=esp}::eDisp immed
		      else
			  modrm{mod=mod, reg=r', rm=base} :: eDisp immed
              in
		  (rex,
		   (case size immed
		     of Zero => 
			if base = esp then 
			    [modrm{mod=0, reg=r', rm=4}, sib{ss=0,index=4,base=esp}]
			else if base = ebp then
			    [modrm{mod=1, reg=r', rm=ebp}, 0w0]
			else 
			    [modrm{mod=0, reg=r', rm=base}]
		      | Bits8 => displace (1, fn i => [toWord8 i])
		      | Bits32 => displace (2, eLong)
		  (*esac*)) )
              end
	    | eImmedExt (r', I.Indexed {base=NONE, index, scale, disp, ...}) = 
              let val rex = (isExtReg' r', isExtReg index, false)
		  val r' = getRO r'
	      in 
		  (rex,
		   (modrm{mod=0, reg=r', rm=4} ::
		    sib{base=5, ss=scale, index=rNum' index} :: 
		    eLong (immedOpnd disp)) )
	      end 
	    | eImmedExt(r', I.Indexed {base=SOME b, index, scale, disp, ...}) = 
	      let val rex = (isExtReg' r', isExtReg index, isExtReg b)
		  val r' = getRO r'
		  val index = rNum' index
		  val base = rNum' b
		  val immed = immedOpnd disp
		  fun indexed (mod, eDisp) = 
		      modrm{mod=mod, reg=r', rm=4} ::
		      sib{ss=scale, index=index, base=base} :: eDisp immed
              in
		  (rex,
		   (case size immed
		     of Zero => 
			if base=ebp then 
			    [modrm{mod=1, reg=r', rm=4},
			     sib{ss=scale, index=index, base=5}, 0w0]
			else
			    [modrm{mod=0, reg=r', rm=4}, 
			     sib{ss=scale, index=index, base=base}]
		      | Bits8 => indexed(1, fn i => [toWord8 i])
		      | Bits32 => indexed(2, eLong)
		   (*esac*)) )
              end
	    | eImmedExt(_, I.Immed _) = error "eImmedExt: Immed"
	    | eImmedExt(_, I.ImmedLabel _) = error "eImmedExt: ImmedLabel"
	    | eImmedExt(_, I.Relative _) = error "eImmedExt: Relative"
	    | eImmedExt(_, I.LabelEA _) = error "eImmedExt: LabelEA"

	  fun encode32' (bytes, r', opnd) = 
	      let val (rex, e) = eImmedExt (r', opnd)
	      in 
		  (case eREXRegs rex
		    of SOME rexByte => (eREX rexByte) :: bytes @ e
		     | NONE => bytes @ e
		  (* esac *))
	      end (* encode32' *)
	  fun encode64' (bytes, r', opnd) =
	      let val (rex, e) = eImmedExt (r', opnd)
	      in 
		  (case eREXRegs rex
		    of SOME rexByte => (eREX64 rexByte) :: bytes @ e
		     | NONE => (eREX64 0wx0) :: bytes @ e
		  (* esac *))
	      end (* encode64' *)
	  fun encode32 (byte1, r', opnd) = eBytes (encode32' ([byte1], r', opnd))
	  fun encode64 (byte1, r', opnd) = eBytes (encode64' ([byte1], r', opnd))
	  fun encode sz = if sz = 64 then encode64 else encode32
	  fun encodeReg32 (byte1, r, opnd) = encode32 (byte1, reg (rNum r), opnd)
	  fun encodeReg64 (byte1, r, opnd) = encode64 (byte1, reg (rNum r), opnd)
	  fun encodeReg sz = if sz = 64 then encodeReg64 else encodeReg32
	  fun encodeLongImm32 (byte1, r', opnd, i) =
	      eBytes ((encode32' ([byte1], r', opnd)) @ eLong i)
	  fun encodeLongImm64 (byte1, r', opnd, i) =
	      eBytes ((encode64' ([byte1], r', opnd)) @ eLong i)
	  fun encodeLongImm sz = if sz = 64 then encodeLongImm64 else encodeLongImm32
	  fun encodeShortImm32 (byte1, r', opnd, w) =
	      eBytes ((encode32' ([byte1], r', opnd)) @ eShort w)
	  fun encodeShortImm64 (byte1, r', opnd, w) =
	      eBytes ((encode64' ([byte1], r', opnd)) @ eShort w)
	  fun encodeShortImm sz = if sz = 64 then encodeShortImm64 else encodeShortImm32
	  fun encodeByteImm32 (byte1, r', opnd, b) =
	      eBytes ((encode32' ([byte1], r', opnd)) @ [toWord8 b])
	  fun encodeByteImm64 (byte1, r', opnd, b) =
	      eBytes ((encode64' ([byte1], r', opnd)) @ [toWord8 b])
	  fun encodeByteImm sz = if sz = 64 then encodeByteImm64 else encodeByteImm32
	  fun encodeST (byte1, opc, STn) = 
	      let fun reg{opc, reg} = W8.fromInt (scale (opc,3) + reg)
	      in eBytes [byte1, reg{opc=opc,reg=fNum STn}] end

	  (* arith: only 5 cases need be considered:
	   *  dst,   src
	   *  -----------
	   *  EAX,   imm32
	   *        r/m32, imm32
	   *  r/m32, imm8
	   *        r/m32, r32
	   *  r32,   r/m32
	   *)
	  fun arith (sz, opc1, opc2) = 
	      let fun f (I.ImmedLabel le, dst) = f(I.Immed(lexp le), dst)
		    | f (I.LabelEA le, dst) = f(I.Immed(lexp le), dst)
		    | f (I.Immed(i), dst) = 
		      (case size i
			of Bits32 => 
			   (case dst
			     of I.Direct (_, r) =>
				if CB.physicalRegisterNum r = eax then 
				    (if sz = 32
				     then eBytes (W8.fromInt (8 * (getRO opc2) + 5) :: eLong(i))
				     else eBytes (eREX64 0w0 :: W8.fromInt(8 * (getRO opc2) + 5) 
						  :: eLong(i)))
				else 
				    encodeLongImm sz (0wx81, opc2, dst, i)
			      | _ => encodeLongImm sz (0wx81, opc2, dst, i)
			   (*esac*))
			 | _ => encodeByteImm sz (0wx83, opc2, dst, i) (* 83 /digit ib *)
		      (*esac*))
		    | f(src, I.Direct (_, r)) = encodeReg sz (opc1+0w3, r, src)
		    | f(I.Direct (_, r), dst) = encodeReg sz (opc1+0w1, r, dst)
		    | f _ = error "arith.f"
	      in f end (* arith *)

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
	  fun test(sz, I.ImmedLabel le, lsrc) = test(sz, I.Immed(lexp le), lsrc)
	    | test(sz, I.LabelEA le, lsrc) = test(sz, I.Immed(lexp le), lsrc)
	    | test(sz, I.Immed(i), lsrc) =
              (case (lsrc, i >= 0 andalso i < 255) of 
		   (I.Direct (_, r), false) => 
		   if CB.physicalRegisterNum r = eax then eBytes(0wxA9 :: eLong i) 
		   else encodeLongImm sz (0wxF7, opcode 0, lsrc, i)
		 | (_, false)  => encodeLongImm sz (0wxF7, opcode 0, lsrc, i)
		 | (I.Direct (_, r), true) =>  (* 8 bit *)
		   let val r = CB.physicalRegisterNum r
		   in  if r = eax then eBytes[0wxA8, toWord8 i]
		       else if r < 4 then 
			   (* unfortunately, only CL, DL, BL can be encoded *)
			   encodeByteImm sz (0wxF6, opcode 0, lsrc, i)
		       else if sz = 8 then error "test.8" 
		       else encodeLongImm sz (0wxF7, opcode 0, lsrc, i)
		   end
		 | (_, true) => encodeByteImm sz (0wxF6, opcode 0, lsrc, i)
              )
	    | test(8, rsrc as I.Direct (_, r), lsrc) =
              if rNum r < 4 then encodeReg32 (0wx84, r, lsrc)
              else error "test.8"
	    | test(sz, I.Direct (_, r), lsrc) = encodeReg sz (0wx85, r, lsrc)
	    | test _ = error "test"

	  fun movsd(byte3, r, opnd) =
	      eBytes (0wxf2::encode32'([0wxf, byte3], reg (rNum r), opnd))

          fun imulq(src,dst) =
              (case (src, dst) 
               of (I.Immed(i), I.Direct (_, dstR)) =>
                  (case size i
                   of Bits32 => 
                      encodeLongImm 64 (0wx69, reg (rNum dstR), dst, i)
                    | _ => encodeByteImm 64 (0wx6b, reg (rNum dstR), dst, i))
                | (_, I.Direct (_, dstR)) => 
                  eBytes (encode64' ([0wx0f, 0wxaf], reg (rNum dstR), src))
                | _ => error "imul")

          (* DEBUG print instructions in stdout
          fun makestream s = let
              fun write f slice = let
                  val t = f slice
                  val _ = s := (!s)^t
                  in
                  String.size t
                  end
              val writer =
                  TextPrimIO.WR
                  {
                   name = "stringout",
                   chunkSize = 512,
                   writeVec = SOME (write CharVectorSlice.vector),
                   writeArr = SOME (write CharArraySlice.vector),
                   writeVecNB = NONE,
                   writeArrNB = NONE,
                   block = NONE,
                   canOutput = NONE,
                   getPos = NONE,
                   setPos = NONE,
                   endPos = NONE,
                   verifyPos = NONE,
                   close = (fn () => ()),
                   ioDesc = NONE
                  }
              val stream =
                  TextIO.StreamIO.mkOutstream (writer, IO.NO_BUF)
              in
              TextIO.mkOutstream stream
              end

          fun instrstring () =
              let
              val s = ref ""
              val stream = makestream s
              val _ =
                  AsmStream.withStream stream
                  (fn _ => let
                      val AsmEmitter.S.STREAM{emit,...} =
                          AsmEmitter.makeStream []
                      in
                      emit (I.INSTR instr) end) ()
              val _ = TextIO.closeOut stream
              in !s end

	  val _ = print (instrstring ())
          *)

      in 
	  (case instr
	    of I.UNARY{unOp, opnd} => 
	       (case unOp
		 of I.INCL => encode32 (0wxff, opcode 0, opnd)
		  | I.INCQ => encode64 (0wxff, opcode 0, opnd)
		  | I.DECL => encode32 (0wxff, opcode 1, opnd)
		  | I.DECQ => encode64 (0wxff, opcode 1, opnd)
		  | I.NOTL => encode32 (0wxff, opcode 2, opnd)
		  | I.NOTQ => encode64 (0wxff, opcode 2, opnd)
		  | I.NEGL => encode32 (0wxff, opcode 3, opnd)
		  | I.NEGQ => encode64 (0wxf7, opcode 3, opnd)
		  | _ => error "UNARY is not in DEC/INC/NEG,NOT"
	       (* esac *))
	     | I.BINARY{binOp, src, dst} => let
		   fun shift (sz, code, src) = 
		       (case src
			 of I.Immed (1) => encode sz (0wxd1, opcode code, dst)
			  | I.Immed (n) => encodeByteImm sz (0wxc1, opcode code, dst, n)
			  | I.Direct (_, r) => 
			    if rNum r <> ecx then  error "shift: Direct"
			    else encode sz (0wxd3, opcode code, dst)
			  (*              | I.MemReg _ => shift(code, memReg src)*)
			  | _  => error "shift"
		       (*esac*))
	       in
		   case binOp
		    of I.ADDL => arith(32, 0w0, opcode 0) (src, dst)
		     | I.SUBL => arith(32, 0wx28, opcode 5) (src, dst)
		     | I.ANDL => arith(32, 0wx20, opcode 4) (src, dst)
		     | I.ORL  => arith(32, 0w8, opcode 1) (src, dst)
		     | I.XORL => arith(32, 0wx30, opcode 6) (src, dst)
		     | I.SHLL => shift(32, 4, src)
		     | I.SARL => shift(32, 7, src)
		     | I.SHRL => shift(32, 5, src)
		     | I.ADDQ => arith(64, 0w0, opcode 0) (src, dst)
		     | I.SUBQ => arith(64, 0wx28, opcode 5) (src, dst)
		     | I.ANDQ => arith(64, 0wx20, opcode 4) (src, dst)
		     | I.ORQ  => arith(64, 0w8, opcode 1) (src, dst)
		     | I.XORQ => arith(64, 0wx30, opcode 6) (src, dst)
		     | I.SHLQ => shift(64, 4, src)
		     | I.SARQ => shift(64, 7, src)
		     | I.SHRQ => shift(64, 5, src)
		     | (I.IMULL | I.MULQ) => 
		       let val sz = if binOp = I.MULQ then 64 else 32
		       in (case (src, dst) 
			    of (I.Immed(i), I.Direct (_, dstR)) =>
			       (case size i
				 of Bits32 => 
				    encodeLongImm sz (0wx69, reg (rNum dstR), dst, i)
				  | _ => encodeByteImm sz (0wx6b, reg (rNum dstR), dst, i)
			       (* esac *))
			     | (_, I.Direct (_, dstR)) => 
			       eBytes (encode32' ([0wx0f, 0wxaf], reg (rNum dstR), src))
			     | _ => error "imul"
			  (* esac *))
		       end 
		     | I.IMULQ => imulq(src,dst)
		     | I.IMULB => error "imulb"
		     | _ => error "binary"
	       end
	     | I.MULTDIV{multDivOp, src} => 
	       let val (mulOp, sz) = 
		       (case multDivOp of
			    I.MULL1 => (4, 32) | I.IDIVL1 => (7, 32) | I.DIVL1 => (6, 32)
			  | I.MULQ1 => (4, 64) | I.IDIVQ1 => (7, 64) | I.DIVQ1 => (6, 64)
			  | I.IMULL1 => error "imull1"
			  | I.IMULQ1 => error "imulq1"
		       (* esac *))
	       in encode sz (0wxf7, opcode mulOp, src)
	       end
	     | I.MUL3{dst, src1, src2=i} => 
	       (case src1 
		 of I.Immed _ => error "mul3: Immed"
		  | I.ImmedLabel _ => error "mul3: ImmedLabel"
		  | _ => 
		    (case size i
		      of Bits32 => encodeLongImm32(0wx69, reg (rNum dst), src1, i)
		       | _ => encodeByteImm32(0wx6b, reg (rNum dst), src1, i)
		    (*esac*))
               (*esac*)) 
	     | I.MULQ3{dst, src1, src2=i} => 
	       (case src1 
		 of I.Immed _ => error "mul3: Immed"
		  | I.ImmedLabel _ => error "mul3: ImmedLabel"
		  | _ => 
		    (case size i
		      of Bits32 => encodeLongImm64(0wx69, reg (rNum dst), src1, i)
		       | _ => encodeByteImm64(0wx6b, reg (rNum dst), src1, i)
		    (*esac*))
               (*esac*)) 

	     | I.RET NONE => eByte 0xc3
	     | I.NOP => eByte 0x90
	     | I.INTO => eByte(0xcd+4)
	     | I.CDQ => (* eByte(0x99) *) (* XXX this is CQO *) eBytes [0wx48, 0wx99]
	     | I.SAHF => eByte(0x9e)
	     | ( I.PUSHL (I.Immed i) | I.PUSHQ (I.Immed i) )=> 
	       (case size i 
		 of Bits32 => eBytes(0wx68 :: eLong(i))
		  | _ => eBytes [0wx6a, toWord8 i]
	       (* esac *))
	     | ( I.PUSHL (I.Direct (_, r)) |
		 I.PUSHQ (I.Direct (_, r)) ) => eByte (0x50+rNum r)
	     | ( I.PUSHL opnd | I.PUSHQ opnd ) => encode32 (0wxff, opcode 6, opnd)
	     | I.POP (I.Direct (_, r)) => eByte (0x58+rNum r)
	     | I.POP opnd => encode32 (0wx8f, opcode 0, opnd)		     
	     | I.LEAL{r32, addr} => encodeReg32(0wx8d, r32, addr)			  
	     | I.LEAQ{r64, addr} => encodeReg64(0wx8d, r64, addr)			  
	     | I.MOVE{mvOp=mvOp as (I.MOVL | I.MOVQ), src, dst} => 
	       let val sz = case mvOp of I.MOVL => 32 | I.MOVQ => 64
		   fun mv(I.Immed(i), I.Direct (_, r)) =
                   (case sz
                    of 32 =>
                       eBytes
                       (Word8.+(0wxb8, Word8.fromInt(rNum r))::eLong(i))
                     | 64 => let
                       val (start, reg) =
                           if rNum r < 8
                           then (0wx48, rNum r)
                           else (0wx49, rNum r - 8)
                           in
                           eBytes(start::0wxc7::Word8.+(0wxc0, Word8.fromInt reg)::eLong(i))
                           end)
		     | mv(I.Immed(i), _) = encodeLongImm sz (0wxc7, opcode 0, dst, i)
		     | mv(I.Immed64(i), I.Direct (_, r)) =
		       if sz = 32
		       then
                       let
                       val (start, reg) =
                           if rNum r < 8
                           then ([], rNum r) else ([0wx41], rNum r - 8)
                       in
                       eBytes(start@Word8.+(0wxb8, Word8.fromInt reg)::eLongLongCut(i))
                       end
		       else
                       let
                       val (start, reg) = if rNum r < 8 then (0wx48, rNum r) else (0wx49, rNum r - 8)
                       in
                       eBytes(start::Word8.+(0wxb8, Word8.fromInt reg)::eLongLong(i))
                       end
		     | mv(I.Immed64(i), _) = error "mv Immed64 _"
		     | mv(I.ImmedLabel le,dst) = mv(I.Immed(lexp le),dst)
		     | mv(I.LabelEA le,dst) = error "MOVL: LabelEA"
		     | mv(src,dst) = arith(sz, 0wx88, opcode 0) (src, dst)
	       in  mv(src,dst) end
	     | I.MOVE{mvOp=I.MOVB, dst, src=I.Immed(i)} =>
	       (case size i
		 of Bits32 => error "MOVE: MOVB: imm8"
		  | _ => encodeByteImm32 (0wxc6, opcode 0, dst, i)
	       (*esac*))
	     | I.MOVE{mvOp=I.MOVB, dst, src=I.Direct (_, r)} => 
	       encodeReg32 (0wx88, r, dst)
	     | I.MOVE{mvOp=I.MOVB, dst=I.Direct (_, r), src} => 
	       encodeReg32 (0wx8a, r, src)
             | I.MOVE{mvOp=I.MOVABSQ, dst, src=I.Immed64 i} => let
               val I.Direct (_, r) = dst
               val (start, reg) =
                   if rNum r < 8 then (0wx48, rNum r) else (0wx49, rNum r - 8)
               in
               eBytes(start::(0wxb8+Word8.fromInt reg)::eLongLong(i))
               end
             | I.MOVE{mvOp=I.MOVABSQ, dst, src} =>
               (case dst
                of I.Direct (_ ,r) =>
                   let
                   val p = rNum r
                   val byte1 = if p < 8 then 0wx48 else 0wx49
                   val byte2 = 0wxb8 + Word8.fromInt (if p < 8 then p else p - 8)
                   val labexp = (case src of I.ImmedLabel x => x
                                           | I.LabelEA _ => (error "MOVABSQc"; Instr.T.???)
                                           | I.Immed _ => (error "MOVABSQd"; Instr.T.???)
                                           | I.Immed64 _ => (error "MOVABSQe"; Instr.T.???)
                                           | _ => (error "MOVABSQa"; Instr.T.???))
                   val byten = eLong (lexp labexp) (* XXX should be 64 bits *)
                   val hilong = if (lexp labexp) < 0 then 
                       [0wxff, 0wxff, 0wxff, 0wxff] else eLong 0
                   in
                   Word8Vector.fromList ([byte1, byte2] @ byten @ hilong)
                   end
                 | _ => (error "MOVABSQb"; Word8Vector.fromList [])
               )
	     | I.MOVE{mvOp, src=I.Immed _, ...} => error "MOVE: Immed"
	     | I.MOVE{mvOp=I.MOVZBQ, src, dst=I.Direct (ty, r)} => let
               val (start, re) =
                   if rNum r < 8 then (0wx48, rNum r) else (0wx49, rNum r - 8)
               in
               eBytes(encode64' ([0wxf, 0wxb6], reg (rNum r), src))
               end
	     | I.MOVE{mvOp, src, dst=I.Direct (_, r)} =>
	       let val byte2 = 
		       case mvOp of
			   I.MOVZBL => 0wxb6 
			 | I.MOVZWL => 0wxb7 
			 | I.MOVSBL => 0wxbe 
			 | I.MOVSWL => 0wxbf 
			 | _ => error "MOV[SZ]X"
	       in eBytes (encode32' ([0wx0f, byte2], reg (rNum r), src)) end
	     | I.MOVE{mvOp=I.MOVW, dst, src=I.Direct (_, r)} =>
               eBytes (0wx66 :: encode32' ([0wx89], reg (rNum r), dst))
	     | I.MOVE _ => error "MOVE"
	     | I.CMOV{cond,src,dst} => 
	       eBytes (encode32' ([0wx0f, Word8.+(condCode cond,0wx40)], 
				  reg (rNum dst), src))
	     | I.JMP(I.Relative i, _) => ((
	       let fun shortJmp () = eBytes [0wxeb, Word8.fromInt (i-2)]
	       in
		   (case size (Int32.fromInt (i-2))
		     of Bits32 => eBytes (0wxe9 :: eLong (Int32.fromInt (i-5)))
		      | _ => shortJmp ()
		   (*esac*))
	       end
	       ) handle e => (print "JMP\n"; raise e))
	     | I.JMP(opnd, _) => let
               val ty = (case opnd of I.Direct (ty,_) => ty | _ => ~1)
               in
                   if ty = 64
                   then
                       let
                       fun encodejmp (bytes, r', opnd) = let
                           val (rex, e) = eImmedExt (r', opnd)
                           in 
                           (case eREXRegs rex
                             of SOME rexByte => (eREX rexByte) :: bytes @ e
                              | NONE => bytes @ e)
                           end
                       in
                           eBytes(encodejmp([0wxff], opcode 4, opnd))
                       end
                   else
                       encode32(0wxff, opcode 4, opnd)
               end
	     | I.JCC{cond, opnd=I.Relative i} => 
	       let val code = condCode cond
	       in case size (Int32.fromInt(i-2))
		   of Bits32 => 
		      eBytes(0wx0f :: Word8.+(0wx80, code) :: 
			     eLong(Int32.fromInt(i-6)))
		    | _ => 
                      eBytes[Word8.+(0wx70,code), Word8.fromInt(i-2)]
	       end 
	     | I.CALL{opnd=I.Relative i,...} => 
	       eBytes (0wxe8 :: eLong (Int32.fromInt (i-5)))
(* FIXME: add CALLQ *)
	     | I.CALL{opnd, ...} => encode32 (0wxff, opcode 2, opnd)
	     | I.CMPL{lsrc, rsrc} => arith(32, 0wx38, opcode 7) (rsrc, lsrc)
	     | I.CMPQ{lsrc, rsrc} => arith(64, 0wx38, opcode 7) (rsrc, lsrc)
	     | (I.CMPW _ | I.CMPB _) => error "CMP"
	     | I.TESTQ{lsrc, rsrc} => test(64, rsrc, lsrc)
	     | I.TESTL{lsrc, rsrc} => test(32, rsrc, lsrc)
	     | I.TESTB{lsrc, rsrc} => test(8, rsrc, lsrc)
	     | I.TESTW _ => error "TEST"
	     | I.SET{cond,opnd} => 
	       eBytes (encode32' ([0wx0f, Word8.+(0wx90,condCode cond)], 
				  reg 0, opnd))
             | I.FMOVE {fmvOp=I.MOVSD, dst=I.FDirect r, src=src} =>
               movsd(0wx10, r, src)
             | I.FMOVE {fmvOp=I.MOVSD, dst=dst, src=I.FDirect r} =>
               movsd(0wx11, r, dst)
             | I.FMOVE {fmvOp, dst, src} =>
               (case fmvOp of
                   I.CVTSI2SD => error "CVTSI2SD not implemented"
                 | I.CVTSI2SDQ => let
                   val I.FDirect r = dst
                   in
                   eBytes([0wxf2] @ encode64'([0wxf,0wx2a],reg (fNum r),src))
                   end
                 | I.CVTSD2SS =>
                   (case dst
                    of (I.FDirect r) =>
                       eBytes([0wxf2] @ encode32'([0wxf,0wx5a],reg (fNum r),src))
                     | _ => error "CVTSD2SS")
                 | I.CVTSS2SD =>
                   (case dst
                    of (I.FDirect r) =>
                       eBytes([0wxf3] @ encode32'([0wxf,0wx5a],reg (fNum r),src))
                     | _ => error "CVTSD2SD")
                 | I.MOVSS =>
                   (case src
                    of (I.FDirect r) =>
                       eBytes([0wxf3] @ encode32'([0wxf,0wx11],reg (fNum r),dst))
                     | _ => error "MOVSS")
                 | _ => error "FMOVE")
	     | I.FBINOP {binOp, src, dst} =>
               (case binOp
                of I.ADDSD =>
                   eBytes([0wxf2] @ encode32'([0wxf,0wx58],reg (fNum dst),src))
                 | I.SUBSD =>
                   eBytes([0wxf2] @ encode32'([0wxf,0wx5c],reg (fNum dst),src))
                 | I.DIVSD =>
                   eBytes([0wxf2] @ encode32'([0wxf,0wx5e],reg (fNum dst),src))
                 | I.MULSD =>
                   eBytes([0wxf2] @ encode32'([0wxf,0wx59],reg (fNum dst),src))
                 | I.XORPD =>
                   eBytes([0wx66] @ encode32'([0wxf,0wx57],reg (fNum dst),src))
                 | I.ANDPD =>
                   eBytes([0wx66] @ encode32'([0wxf,0wx54],reg (fNum dst),src))
                 | _ => error "FBINOP")
	     | I.FCOM {comOp, src, dst} =>
               (case comOp
                of I.UCOMISD =>
                   eBytes([0wx66] @ encode32'([0wxf,0wx2e],reg (fNum dst),src))
                 | _ => error "FCOMOP")
             | I.FSQRTS {dst, src} => let
               val I.FDirect r = dst
               in
                   eBytes([0wxf3] @ encode32'([0wxf,0wx51],reg (fNum r),src))
               end
             | I.FSQRTD {dst, src} => let
               val I.FDirect r = dst
               in
                   eBytes([0wxf2] @ encode32'([0wxf,0wx51],reg (fNum r),src))
               end
	     | _ => error "emitInstr"
	  (* esac *))
      end (* emitAMD64Instr *)
	   
  and emitInstr (I.LIVE _) = Word8Vector.fromList []
    | emitInstr (I.KILL _) = Word8Vector.fromList []
    | emitInstr(I.COPY{k, dst, src, tmp, ...}) = 
      (case k 
	of CB.GP => emitInstrs (Shuffle.shuffle {tmp=tmp, dst=dst, src=src})
	 | CB.FP => emitInstrs (Shuffle.shufflefp {tmp=tmp, dst=dst, src=src})
	 | _ => error "COPY"
      (*esac*))
    | emitInstr (I.INSTR instr) = emitAMD64Instr instr
    | emitInstr (I.ANNOTATION{i,...}) = emitInstr i

end (* AMD64MCEmitter *)
