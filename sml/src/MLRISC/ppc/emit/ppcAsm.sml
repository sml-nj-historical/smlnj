(* ppcAsm.sml
 *
 * COPYRIGHT (c) 1999 Lucent Technologies, Bell Laboratories.
 *
 *)

functor PPCAsmEmitter
  (structure Instr : PPCINSTR
   structure PseudoOps : PSEUDO_OPS 
   structure Shuffle : PPCSHUFFLE where I = Instr)  : EMITTER_NEW = 
struct
  structure I = Instr
  structure C = I.C
  structure P = PseudoOps
  structure R = I.Region

  structure Constant = I.Constant

  fun error msg = MLRiscErrorMsg.impossible ("PPCAsmEmitter." ^ msg)
  fun ms n = if n<0 then ("-" ^ Int.toString (~n)) else Int.toString n
  fun emit s = TextIO.output(!AsmStream.asmOutStream,s)
  fun pseudoOp pOp = emit(P.toString pOp)
  fun defineLabel(lab) = emit(Label.nameOf lab ^ ":\n")
  fun comment msg = emit ("\t/* " ^ msg ^ " */")
  fun init size = comment ("Code Size = " ^ ms size)
  fun emitInstr(instr,regmap) = let
    fun rmap r		= (Intmap.map regmap r) handle _ => r
    fun eReg (i)	= emit (Int.toString(rmap i))
    fun eFreg(f)       	= emit(Int.toString(rmap f))
    fun eLabel lab     	= emit (Label.nameOf lab)
    fun newline ()     	= emit "\n"
    fun comma() 	= emit ", "
    fun tab()           = emit "\t"
    fun emitLExp lexp = emit(LabelExp.toString lexp)

    fun parenthesize f = (emit "("; f(); emit ")")

    fun eOperand(I.RegOp r) = eReg r
      | eOperand(I.ImmedOp i) = emit(ms i)
      | eOperand(I.LabelOp lexp) = emitLExp lexp
      | eOperand(I.ConstOp c) = emit(Constant.toString c)
    fun eDisp(rd, disp)	= (eOperand(disp); parenthesize (fn () => eReg rd))
    fun eOERc{OE=false, Rc=false} = ""
      | eOERc{OE=false, Rc=true}  = "."
      | eOERc{OE=true,  Rc=false} = "o"
      | eOERc{OE=true,  Rc=true}  = "o." 
    fun eRc false = ""
      | eRc true = "."
    fun rrr(oper, rt, ra, rb) = 
      (emit oper; tab(); eReg rt; comma(); eReg ra; comma(); eReg rb)
    fun rrrRc(oper, rt, ra, rb, Rc) = 
      (emit oper; eRc Rc; tab(); 
       eReg rt; comma(); eReg ra; comma(); eReg rb)
    fun rdr(oper, rt, ra, d) = 
      (emit oper; tab(); eReg rt; comma(); eOperand d; 
       parenthesize(fn () => eReg ra))
    fun emitBO bo = 
      emit(case bo
	of I.TRUE => "t"
         | I.FALSE => "f"
	 | I.ALWAYS => ""
         | I.COUNTER{eqZero, cond=NONE} => if eqZero then "dz" else "dnz"
	 | I.COUNTER{eqZero, cond=SOME cc} => 
	     (if eqZero then "dz" else "dnz") ^ 
		(if cc then "t" else "f")
	(*esac*))

    fun bitToString I.LT = "lt"
      | bitToString I.GT = "gt"
      | bitToString I.EQ = "eq"
      | bitToString I.SO = "so"	
      | bitToString I.FL = "lt"
      | bitToString I.FG = "gt"
      | bitToString I.FE = "eq"
      | bitToString I.FU = "un"
      (* as far as I can tell there don't seem to be mnemonics
       * for these, however using lt, gt, eq, so should fool
       * the assembler into looking at the right bits in the
       * cc field. Of course the bf field had better be cr1.
       *)
      | bitToString I.FX = "lt"
      | bitToString I.FEX = "gt"
      | bitToString I.VX = "eq"
      | bitToString I.OX = "so"

    fun emitBI(bo, bf, bit, more) = let
      fun comma() = emit(if more then "," else "")
    in
      case (bo, bf)
      of (I.ALWAYS, _) => ()
       | (I.COUNTER{cond=NONE, ...}, _) => ()
       | _ => 
	  (case bf
	   of 0 => 
	       (emit(bitToString bit); comma())
	    | n => 
	       (emit("4*cr" ^ Int.toString n ^ "+" ^ bitToString bit); 
		comma())
          (*esac*))
      (*esac*)
    end

    fun rrOERc(oper, rt, ra, OE, Rc) = 
      (emit oper; emit(eOERc{OE=OE, Rc=Rc}); tab(); 
       eReg rt; comma(); eReg ra)
    fun rrrOERc(oper, rt, ra, rb, OE, Rc) = 
      (emit oper; emit(eOERc{OE=OE, Rc=Rc}); tab();
       eReg rt; comma(); eReg ra; comma(); eReg rb)
    fun rri(oper, rt, ra, si) = 
      (emit oper; tab(); eReg rt; comma(); eReg ra; comma(); eOperand si)

    fun rotate(oper, ra, rs, sh, mb, me) =
        (emit oper; tab(); eReg ra; comma(); eReg rs; comma(); 
	 case sh
	  of I.RegOp rb => eReg rb
	   |  _ => eOperand sh
	 (*esac*);
	 comma(); emit(Int.toString mb); comma(); 
	 emit(Int.toString me)) 
    fun fcmp(oper, bf, fa, fb) = 
      (emit oper; tab(); 
       emit(Int.toString bf); comma(); eFreg fa; comma(); eFreg fb)
    fun frr(oper, ft, ra, rb) = 
      (emit oper; tab(); eFreg ft; comma(); eReg ra; comma(); eReg rb)
    fun fdr(oper, ft, ra, d) = 
      (emit oper; tab(); eFreg ft; comma(); eOperand d; 
       parenthesize(fn () => eReg ra))
    fun ff(oper, fa, fb, Rc) = 
      (emit oper; emit(if Rc then "." else "");
       tab(); eFreg fa; comma(); eFreg fb)
    fun fff(oper, ft, fa, fb, Rc) = 
      (emit oper; emit(if Rc then "." else "");
       tab(); eFreg ft; comma(); eFreg fa; comma(); eFreg fb)
    fun cr_bit(cr, bit) = 
      4 * cr + (case bit
	of I.LT => 0
         | I.GT => 1
	 | I.EQ => 2
	 | I.SO => 3

	 | I.FL => 0
         | I.FG => 1
         | I.FE => 2
         | I.FU => 3

         | I.FX =>  0
         | I.FEX => 1
         | I.VX =>  2
         | I.OX =>  3
       (*esac*))

    fun ccc(oper, ct, ca, cb) = 
      (emit oper; tab(); 
       emit(Int.toString(cr_bit ct)); comma();
       emit(Int.toString(cr_bit ca)); comma();
       emit(Int.toString(cr_bit cb)))
  in
    tab();
    case instr
    of I.L{sz, rt, ra, d=I.RegOp rb, mem} => 
	(case sz 
	 of I.Byte => rrr("lbzx", rt, ra, rb)
	  | I.Half => rrr("lhzx", rt, ra, rb)
	  | I.Word => rrr("lwzx", rt, ra, rb)
	  | I.Long => rrr("ldx", rt, ra, rb)
	  | I.Single => frr("lfsx", rt, ra, rb)
	  | I.Double => frr("lfdx", rt, ra, rb)
	 (*esac*))
     | I.L{sz, rt, ra, d, mem} => 
	(case sz 
	 of I.Byte => rdr("lbz", rt, ra, d)
	  | I.Half => rdr("lhz", rt, ra, d)
	  | I.Word => rdr("lwz", rt, ra, d)
	  | I.Long => rdr("ld", rt, ra, d)
	  | I.Single => fdr("lfs", rt, ra, d)
	  | I.Double => fdr("lfd", rt, ra, d)
	 (*esac*))
     | I.ST{sz, rs, ra, d=I.RegOp rb, mem} => 
	(case sz 
	 of I.Byte => rrr("stbx", rs, ra, rb)
	  | I.Half => rrr("sthx", rs, ra, rb)
	  | I.Word => rrr("stwx", rs, ra, rb)
	  | I.Long => rrr("stdx", rs, ra, rb)
	  | I.Single => frr("stfsx", rs, ra, rb)
	  | I.Double => frr("stfdx", rs, ra, rb)
	 (*esac*))
     | I.ST{sz, rs, ra, d, mem} => 
	(case sz 
	 of I.Byte => rdr("stb", rs, ra, d)
	  | I.Half => rdr("sth", rs, ra, d)
	  | I.Word => rdr("stw", rs, ra, d)
	  | I.Long => rdr("std", rs, ra, d)
	  | I.Single => fdr("stfs", rs, ra, d)
	  | I.Double => fdr("stfd", rs, ra, d)
	 (*esac*))
     | I.UNARY{oper,rt,ra,Rc,OE} => 
	(case oper
	  of I.NEG => rrOERc("neg", rt, ra, OE, Rc)
        (*esac*))
     | I.ARITH{oper,rt,ra,rb,Rc,OE} => 
	(case oper
	  of I.ADD => rrrOERc("add", rt, ra, rb, OE,  Rc)
	   | I.ADDS => error "emitInstr:ARITH:ADDS"
           | I.SUBF => rrrOERc("subf", rt, ra, rb, OE, Rc)
	   | I.MULL => rrrOERc("mullw", rt, ra, rb, OE, Rc)
	   | I.DIVW => rrrOERc("divw", rt, ra, rb, OE, Rc)
	   | I.DIVWU => rrrOERc("divwu", rt, ra, rb, OE, Rc)
	   | I.AND => rrrRc("and", rt, ra, rb, Rc)
	   | I.OR => rrrRc("or", rt, ra, rb, Rc)
	   | I.XOR => rrrRc("xor", rt, ra, rb, Rc)
	   | I.SLW => rrrRc("slw", rt, ra, rb, Rc)
	   | I.SRW => rrrRc("srw", rt, ra, rb, Rc)
	   | I.SRAW => rrrRc("sraw", rt, ra, rb, Rc)
	   | I.XORS => error "emitInstr:ARITH:XORS"
	(*esac*))
     | I.ARITHI{oper, rt, ra, im} =>
	(case oper
	  of I.ADD => rri("addi", rt, ra, im)
	   | I.ADDS => rri("addis", rt, ra, im)
           | I.SUBF => rri("subfic", rt, ra, im)
	   | I.MULL => rri("mulli", rt, ra, im)
	   | I.DIVW => error "emitInstr:ARITHI:DIVW"
	   | I.DIVWU => error "emitInstr:ARITHI:DIVWU"
	   | I.AND => rri("andi.", rt, ra, im)
	   | I.OR => rri("ori", rt, ra, im)
	   | I.XOR => rri("xori", rt, ra, im)
	   | I.XORS => rri("xoris", rt, ra, im)
	   | I.SLW => rri("slwi", rt, ra, im)
	   | I.SRW => rri("srwi", rt, ra, im)
	   | I.SRAW => rri("srawi", rt, ra, im)
	(*esac*))
     | I.ROTATE{oper, ra, rs, sh, mb, me} =>
	(case sh 
	  of I.RegOp rb => rotate("rlwinm", ra, rs, sh, mb, me)
	   | _ => rotate("rlwnm", ra, rs, sh, mb, me)
	(*esac*))   
     | I.COMPARE{cmp, bf, ra, rb} => 
       (case cmp
	 of I.CMP => emit "cmp"		
	  | I.CMPL => emit "cmpl"
	(*esac*);
	case rb
	 of I.RegOp _ => ()
	  | _ => emit "i"
	(*esac*);
	tab(); emit(Int.toString bf); comma();
	eReg ra; comma(); eOperand rb
       (*esac*))
     | I.FCOMPARE{cmp, bf, fa, fb} =>
	(case cmp
	 of I.FCMPO => fcmp("fcmpo", bf, fa, fb)
          | I.FCMPU => fcmp("fcmpu", bf, fa, fb)
	(*esac*))
     | I.FUNARY{oper, ft, fb, Rc} =>
	(case oper
         of I.FMR => ff("fmr", ft, fb, Rc)
          | I.FABS => ff("fabs", ft, fb, Rc)
          | I.FNEG => ff("fneg", ft, fb, Rc)
	(*esac*))
     | I.FARITH{oper, ft, fa, fb, Rc} =>
	(case oper
	 of I.FADD => fff("fadd", ft, fa, fb, Rc)
          | I.FSUB => fff("fsub", ft, fa, fb, Rc)
          | I.FMUL => fff("fmul", ft, fa, fb, Rc)
          | I.FDIV => fff("fdiv", ft, fa, fb, Rc)
	 (*esac*))
     | I.CCARITH{oper, bt, ba, bb} =>
	(case oper
	 of I.CRAND => ccc("crand", bt, ba, bb)
	  | I.CROR => ccc("cror", bt, ba, bb)
	  | I.CRXOR => ccc("crxor", bt, ba, bb)
	  | I.CRNAND => ccc("crnand", bt, ba, bb)
	  | I.CRNOR => ccc("crnor", bt, ba, bb)
	 (*esac*))
     | I.MCRF{bf:int, bfa:int} =>
	 (emit "mcrf"; tab(); emit(Int.toString bf); comma();
	  emit(Int.toString bfa))
     | I.MTSPR{rs:int, spr} =>
	 (emit(case spr
	    of 1 => "mtxer"
	     | 8 => "mtlr"
	     | 9 => "mtctr"
	     | _ => error "mtspr"
            (*esac*));
	  tab(); eReg(rs))
     | I.MFSPR{rt:int, spr} => 
	 (emit(case spr
	    of 1 => "mfxer"
	     | 8 => "mflr"
	     | 9 => "mfctr"
	     | _ => error "mtspr"
            (*esac*));
	  tab(); eReg(rt))
      | I.TWI{to, ra, si} =>
	 (emit "twi\t"; emit(Int.toString to); comma();
	  eReg ra; comma(); eOperand si) 
    (* Control Instructions -  AA is always assumed to be 0 *)
     | I.BC{bo, bf, bit, addr, LK, ...} => 
	 (emit "b";
	  emitBO bo;
          emit(if LK then "l" else "");
	  tab();
	  emitBI(bo, bf, bit, true); eOperand addr)
     | I.BCLR{bo, bf, bit, LK, labels} =>
	 (emit "b";
	  emitBO bo;
	  emit "lr";
          emit(if LK then "l" else "");
	  tab();
	  emitBI(bo, bf, bit, false))
      | I.B{addr, LK} => 
	 (emit "b"; emit(if LK then "l" else ""); tab(); eOperand addr)
	  
    (* CALL = BCLR {bo=ALWAYS, bf=0, bit=0, LK=true, labels=[] *)
     | I.CALL{def, use} => emit("bclr")

     | I.COPY{dst, src, impl, tmp} =>
	  app (fn instr => (emit "\t"; emitInstr(instr, regmap)))
	      (Shuffle.shuffle
	         {regMap=rmap, temp=tmp, dst=dst, src=src})
     | I.FCOPY{dst, src, impl, tmp} =>
	  app (fn I => (emit "\t"; emitInstr(I, regmap)))
	      (Shuffle.shufflefp
   	         {regMap=rmap, temp=tmp, dst=dst, src=src})
    (*esac*);
    emit("\n")
  end (*emitInstr*)
end
