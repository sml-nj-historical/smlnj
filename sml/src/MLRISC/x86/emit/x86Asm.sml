(* x86Asm.sml -- generates GAS compatible x86 assembler
 *
 * COPYRIGHT (c) 1997 Bell Laboratories.
 *
 * $Log: X86Asm.sml,v $
 * Revision 1.2  1998/08/27 14:12:11  george
 *   used sethi-ullman number for floating point stack
 *
 * Revision 1.1.1.1  1998/07/22 18:10:32  george
 *   X86.1
 *
 *)

(* Linux uses the AT&T syntax and not the intel syntax,
 * (see Using as: The GNU Assembler)
 * memory operands have the form:
 *		section:disp(base, index, scale)
 * scale in {1,2,4,8}
 *)

functor X86AsmEmitter
  (structure Instr : X86INSTR
   structure PseudoOps : PSEUDO_OPS
   structure MemRegs : MEMORY_REGISTERS where I = Instr
   structure Shuffle : X86SHUFFLE where I = Instr) = 
struct
  structure I = Instr
  structure P = PseudoOps
  structure Constant = I.Constant
  structure LE = LabelExp
  structure C = I.C

  fun error msg = MLRiscErrorMsg.impossible ("X86AsmEmitter." ^ msg)

  fun iToString n = if n<0 then ("-"^Int.toString (~n)) else Int.toString n
  fun i32ToString n = let
    val minInt = ~2147483648
  in
    if n=minInt then "-2147483648" 
    else if n<0 then ("-"^Int32.toString (~n)) else Int32.toString n
  end
  fun emit s = TextIO.output(!AsmStream.asmOutStream,s)
  fun pseudoOp pOp = emit(P.toString pOp)
  fun defineLabel(lab) = emit(Label.nameOf lab ^ ":\n")
  fun comment msg = emit ("/*" ^ msg ^ "*/\n")
  fun init size = comment ("Code Size = " ^ iToString size)

  fun emitInstr(instr,regmap) = let
    fun rmap r = (Intmap.map regmap r) handle _ => r
    val fmap = rmap
    val memReg = MemRegs.memReg rmap
      
    fun symbolicReg 0 = "%eax"
      | symbolicReg 1 = "%ecx"
      | symbolicReg 2 = "%edx"
      | symbolicReg 3 = "%ebx"
      | symbolicReg 4 = "%esp"
      | symbolicReg 5 = "%ebp"
      | symbolicReg 6 = "%esi"
      | symbolicReg 7 = "%edi"
      | symbolicReg r = let
          val r' = rmap r
        in if r <> r' then symbolicReg r' else "%" ^ iToString r'
	end
    fun comma()	 = emit ", "
    fun tab()  	 = emit "\t"
    fun eReg (r) = emit (symbolicReg r)
    fun eFreg(f) = emit("%st(" ^ iToString(fmap f) ^ ")")

    fun eLabel lab = emit (Label.nameOf lab ^ ":")
    fun newline () = emit "\n"
    fun brackets f = (emit "("; f (); emit ")")

    fun emitLExp lexp = emit(LabelExp.toString lexp)
         
    fun eImmed(I.Immed (i)) = emit(i32ToString i)
      | eImmed(I.Const c) = emit(Constant.toString c)
      | eImmed(I.ImmedLabel lexp) = emitLExp lexp
      | eImmed _ = error "eImmed"

    fun scaleMultiplier 0 = 1
      | scaleMultiplier 1 = 2
      | scaleMultiplier 2 = 4
      | scaleMultiplier 3 = 8
      | scaleMultiplier _ = error "scaleMultiplier"

    fun eOperand(i as I.Immed _) = (emit "$"; eImmed i)
      | eOperand(I.Const c) = emit(Constant.toString c)
      | eOperand(lab as I.ImmedLabel _) = (emit "$"; eImmed lab)
      | eOperand(I.Direct r) = eReg r
      | eOperand(f as I.FDirect fr) = let
	  val f' = fmap fr
        in if f' < 8 then eFreg f' else eOperand(memReg f)
	end
      | eOperand(I.LabelEA lexp) = emitLExp lexp
      | eOperand(I.Displace{base, disp=I.Immed(0)}) = 
         brackets(fn () => eReg base)
      | eOperand(I.Displace{base, disp}) = 
         (eImmed disp; brackets (fn () => eReg base))
      | eOperand(I.Indexed{base=NONE, index, scale, disp}) = 
	 (eOptionalDisp disp;
	  brackets(fn() => (comma(); eReg index; comma(); 
			    emit(iToString (scaleMultiplier scale))))) 
      | eOperand(I.Indexed{base=SOME b, index, scale, disp}) =
	 (eOptionalDisp disp;
	  brackets
	    (fn() => (eReg b; comma(); eReg index; comma(); 
		      emit(iToString (scaleMultiplier scale))))) 

    and eOptionalDisp(I.Immed(0)) = ()
      | eOptionalDisp(I.Const c) = emit(Constant.toString c)
      | eOptionalDisp(opnd as I.Immed _) = eImmed opnd
      | eOptionalDisp _ = error "eOptionalDisp"

    (* The gas assembler does not like the "$" prefix for immediate
     * labels in certain instructions. 
     *)
    fun stupidGas(I.ImmedLabel lexp) = emitLExp lexp
      | stupidGas(I.LabelEA _) = error "stupidGas"
      | stupidGas opnd = eOperand opnd
   
  in
    case instr 
     of I.NOP => emit  "\tnop"
      | I.JMP(opnd, _) => (emit"\tjmp\t"; stupidGas opnd)
      | I.JCC{cond, opnd} => 
         (emit
	   (case cond
	     of I.EQ => "\tje\t" | I.NE => "\tjne\t" | I.LT => "\tjl\t"
	      | I.LE => "\tjle\t" | I.GT => "\tjg\t" | I.GE => "\tjge\t"
	      | I.B => "\tjb\t" | I.BE => "\tjbe\t" | I.A => "\tja\t"
	      | I.AE => "\tjae\t" | I.C => "\tjc\t" | I.NC => "\tjnc\t"
	      | I.P => "\tjp\t" | I.NP => "\tjnp\t" | I.O => "\tjo\t"
	      | I.NO => "\tjno\t"
	    (*esac*));
	  stupidGas opnd)
      | I.CALL(opnd,d,u) => (emit "\tcall\t"; stupidGas opnd)
			 (*    emit ("defs=" ^ C.cellset2string d);
			     emit (" uses=" ^ C.cellset2string u)) *)
      | I.RET  => emit "\tret"
      | I.MOVE{mvOp, src, dst} => 
	 (emit
	   (case mvOp
	     of I.MOVL => "\tmovl\t"
	      | I.MOVZX => "\tmovzx\t"
	      | I.MOVB => "\tmovb\t");
	      eOperand src; comma(); eOperand dst)
      | I.LEA{r32, addr} => 
	 (emit "\tleal\t"; eOperand addr; comma(); eReg r32)
      | I.CMP{lsrc, rsrc} => 
	 (emit "\tcmpl\t"; eOperand rsrc; comma(); eOperand lsrc)
      | I.BINARY{binOp, src, dst} =>
	 (emit
	   (case binOp
	     of I.ADD => "\taddl\t"
	      | I.SUB => "\tsubl\t"
              | I.AND => "\tandl\t"
	      | I.OR  => "\torl\t"
	      | I.XOR => "\txorl\t"
	      | I.SHL => "\tshll\t"
	      | I.SAR => "\tsarl\t"
	      | I.SHR => "\tshrl\t");
	  eOperand src; comma();  eOperand dst)
      | I.MULTDIV{multDivOp, src} =>
	 (emit
	   (case multDivOp
	     of I.IDIV => "\tidivl\t"
              | I.UDIV => "\tdivl\t"
              | I.UMUL => "\tmull\t"
	    (*esac*));
	  eOperand src)
      | I.MUL3{src1, src2, dst} => 
	(emit "\timul\t";
	 case src2
	 of NONE => () | SOME i => (eOperand(I.Immed(i)); comma())
	 (*esac*);
	 eOperand src1; 
	 comma();
	 eOperand(I.Direct dst))
      | I.UNARY{unOp, opnd} => 
	 (emit
	   (case unOp
	      of I.DEC => "\tdecl\t" 
	       | I.INC => "\tincl\t" 
	       | I.NEG => "\tnegl\t" 
	       | I.NOT => "\tnotl\t");
	  eOperand opnd)
      | I.PUSH opnd => (emit("\tpushl\t"); eOperand opnd)
      | I.POP opnd => (emit("\tpopl\t"); eOperand opnd)
      | I.CDQ => emit "\tcdq"
      | I.INTO => emit "\tinto"

      | I.COPY{dst, src, tmp, ...} => 
	  app (fn I => (emit "\t"; emitInstr(I, regmap)))
	      (Shuffle.shuffle {regMap=rmap, temp=tmp, dst=dst, src=src})

      | I.FCOPY{dst, src, tmp, ...} => 
	  app (fn I => (emit "\t"; emitInstr(I, regmap)))
	      (Shuffle.shufflefp{regMap=fmap, temp=tmp, dst=dst, src=src})

      | I.FBINARY{binOp, src, dst} => 
	(emit (case binOp
	   of I.FADDP => "\tfaddp\t"   | I.FADD => "\tfadd\t"
	    | I.FSUB => "\tfsub\t"     | I.FSUBP => "\tfsubp\t"
	    | I.FSUBR => "\tfsubr\t"   | I.FSUBRP => "\tfsubrp\t"
	    | I.FMUL => "\tfmul\t"     | I.FMULP => "\tfmulp\t"
	    | I.FDIV => "\tfdiv\t"     | I.FDIVP => "\tfdivp\t"
	    | I.FDIVR => "\tfdivr\t"   | I.FDIVRP => "\tfdivrp\t"
	  (*esac*));
	case (src, dst) 
	of (I.FDirect f, _) => 
	    if fmap f > 7 then eOperand src (* memory location *)
	    else (eOperand src; comma(); eOperand dst)
         | (_, I.FDirect 0) => eOperand src
	(*esac*))
      | I.FUNARY I.FABS => emit "\tfabs"
      | I.FUNARY I.FCHS => emit "\tfchs"
      | I.FXCH => emit "\tfxch"
      | I.FUCOMPP => emit "\tfucompp\t"
      | I.FSTP opnd => (emit "\tfstp\t"; eOperand opnd)
      | I.FLD(f as I.FDirect _) => emitInstr(I.FLD(memReg f), regmap)
      | I.FLD opnd => (emit "\tfld\t"; eOperand opnd)
      | I.FILD opnd =>(emit "\tfild\t"; eOperand opnd)
      | I.FNSTSW => emit "\tfnstsw"
      | I.SAHF => emit "\tsahf"
    (*esac*);
    emit "\n"
  end
end


