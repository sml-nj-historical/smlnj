functor PPCProps
   ( structure PPCInstr : PPCINSTR
     structure MLTreeEval : MLTREE_EVAL where T = PPCInstr.T
     structure MLTreeHash : MLTREE_HASH where T = PPCInstr.T
    ) : INSN_PROPERTIES = 
struct
  structure I = PPCInstr
  structure C = I.C
  structure T = I.T 
  structure CB = CellsBasis

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("PPCProps",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  (* This stupid architecture doesn't really have a dedicated zero register *)
  fun zeroR() = C.Reg CB.GP 0

  fun instrKind(I.ANNOTATION{i, ...}) = instrKind i
    | instrKind(I.INSTR instr) = 
	(case instr
	 of (I.BC _) => IK_JUMP
	  | (I.BCLR _) => IK_JUMP
	  | (I.B _) => IK_JUMP
	  | (I.ARITHI{oper=I.ORI, rt, ra, im=I.ImmedOp 0}) => 
	       if CB.registerId rt = 0 andalso CB.registerId ra = 0 then IK_NOP
	       else IK_INSTR
	  | (I.COPY _) => IK_COPY
	  | (I.FCOPY _)			=> IK_COPY
	  | (I.CALL{cutsTo=_::_,...}) => IK_CALL_WITH_CUTS
	  | (I.CALL _) => IK_CALL
	  | (I.PHI _)    => IK_PHI
	  | (I.SOURCE _) => IK_SOURCE
	  | (I.SINK _)   => IK_SINK
	  |  _ => IK_INSTR)
    | instrKind _ = error "instrKind"

  fun moveInstr(I.INSTR(I.COPY _)) = true
    | moveInstr(I.INSTR(I.FCOPY _)) = true
    | moveInstr(I.LIVE _) = false 
    | moveInstr(I.KILL _) = false 
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr  _ = false

  fun nop () = I.arithi{oper=I.ORI, rt=zeroR(), ra=zeroR(), im=I.ImmedOp 0}

  fun moveTmpR(I.INSTR(I.COPY{tmp as SOME(I.Direct r), ...})) = SOME r
    | moveTmpR(I.INSTR(I.FCOPY{tmp as SOME(I.FDirect f), ...})) = SOME f
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR _ = NONE

  fun moveDstSrc(I.INSTR(I.COPY{src, dst, ...})) = (dst, src)
    | moveDstSrc(I.INSTR(I.FCOPY{src, dst, ...})) = (dst, src)
    | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc _ = error "moveDstSrc"

  fun branchTargets(I.INSTR(I.BC{bo=I.ALWAYS, addr,  ...})) = 
      (case addr
        of I.LabelOp(T.LABEL lab) => [LABELLED lab]
         | _ => error "branchTargets:BC:ALWAYS"
      (*esac*))
    | branchTargets(I.INSTR(I.BC{addr, ...})) = 
      (case addr
        of I.LabelOp(T.LABEL lab) => [LABELLED lab, FALLTHROUGH]
         | _ => error "branchTargets:BC"
      (*esac*))
    | branchTargets(I.INSTR(I.BCLR{labels, bo=I.ALWAYS, ...})) = 
      (case labels of [] => [ESCAPES] | _ => map LABELLED labels)
    | branchTargets(I.INSTR(I.BCLR{labels,  ...})) = 
      (case labels of [] => [ESCAPES, FALLTHROUGH] | _ => map LABELLED labels)
    | branchTargets(I.INSTR(I.B{addr=I.LabelOp(T.LABEL lab), LK})) = [LABELLED lab]
    | branchTargets(I.INSTR(I.CALL{cutsTo, ...})) = FALLTHROUGH::map LABELLED cutsTo
    | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets _ = error "branchTargets"

  fun jump lab = I.b{addr=I.LabelOp(T.LABEL lab), LK=false}

  val immedRange = {lo= ~32768, hi=32767}

  fun loadImmed{immed,t} = 
       I.arithi
         {oper=I.ADDI, rt=t, ra=zeroR(), 
          im=if #lo immedRange <= immed andalso immed <= #hi immedRange
             then I.ImmedOp immed else I.LabelOp(I.T.LI(IntInf.fromInt immed))}
  fun loadOperand{opn,t} = 
       I.arithi{oper=I.ADDI, rt=t, ra=zeroR(), im=opn}

  fun setTargets _ = error " setTargets"

  fun negateConditional _ = error "negateConditional"

  fun hashOpn(I.RegOp r) = CB.hashCell r
    | hashOpn(I.ImmedOp i) = Word.fromInt i
    | hashOpn(I.LabelOp l) = MLTreeHash.hash l
  fun eqOpn(I.RegOp a,I.RegOp b) = CB.sameColor(a,b)
    | eqOpn(I.ImmedOp a,I.ImmedOp b) = a = b
    | eqOpn(I.LabelOp a,I.LabelOp b) = MLTreeEval.==(a,b)
    | eqOpn _ = false

  fun defUseR instr = let
    fun ppcDU instr = let
      fun operand(I.RegOp r,use) = r::use
	| operand(_,use) = use
    in
      case instr
      of I.L{rt, ra, d, ...} => ([rt], operand(d,[ra]))
       | I.LF{ra, d, ...} => ([], operand(d,[ra]))
       | I.ST{rs, ra, d, ...} => ([], operand(d,[rs,ra]))
       | I.STF{ra, d, ...} => ([], operand(d,[ra]))
       | I.UNARY{rt, ra, ...} => ([rt], [ra])
       | I.ARITH{rt, ra, rb, ...} => ([rt], [ra,rb])
       | I.ARITHI{rt, ra, im, ...} => ([rt], operand(im,[ra]))
       | I.ROTATE{ra, rs, sh, ...} => ([ra], [rs,sh])
       | I.ROTATEI{ra, rs, sh, ...} => ([ra], operand(sh,[rs]))
       | I.COMPARE{ra, rb, ...} => ([], operand(rb,[ra]))
       | I.MTSPR{rs, ...} => ([], [rs])
       | I.MFSPR{rt, ...} => ([rt], [])
       | I.TW{to, ra, si} => ([], operand(si,[ra]))
       | I.TD{to, ra, si} => ([], operand(si,[ra]))
       | I.CALL{def, use, ...} => (C.getReg def, C.getReg use)
       | I.COPY{dst, src, tmp, ...} => 
	  (case tmp
	    of NONE => (dst, src)
	     | SOME(I.Direct r) => (r::dst, src)
	  (* | SOME(I.Displace{base, disp}) => (dst, base::src) *)
	   (*esac*))
       | _ => ([], [])
    end
  in
    case instr
     of I.ANNOTATION{i, ...} => defUseR i
      | I.LIVE{regs, ...} => ([], C.getReg regs)
      | I.KILL{regs, ...} => (C.getReg regs, [])
      | I.INSTR(i) => ppcDU(i)
      | _ => error "defUseR"
  end

  fun defUseF instr = let
    fun ppcDU instr = 
     (case instr
      of I.LF{ft, ...} => ([ft],[])
       | I.STF{fs, ...} => ([], [fs])
       | I.FCOMPARE{fa, fb, ...}  => ([], [fa, fb])
       | I.FUNARY{ft, fb, ...}  => ([ft], [fb])
       | I.FARITH{ft, fa, fb, ...}  => ([ft], [fa, fb])
       | I.FARITH3{ft, fa, fb, fc, ...}  => ([ft], [fa, fb, fc])
       | I.CALL{def, use, ...} => (C.getFreg def,C.getFreg use)
       | I.FCOPY{dst, src, tmp, ...} => 
	  (case tmp
	    of SOME(I.FDirect f) => (f::dst, src)
	     | _ => (dst, src)
	   (*esac*))
       | _ => ([], [])
      (*esac*))
  in
    case instr
     of I.ANNOTATION{i, ...} => defUseF i
      | I.LIVE{regs, ...} => ([], C.getFreg regs)
      | I.KILL{regs, ...} => (C.getFreg regs, [])
      | I.INSTR(i) => ppcDU(i)
      | _ => error "defUseF"

  end
  fun defUseCC instr = error "defUseCC: not implemented"

  fun defUse CB.GP = defUseR
    | defUse CB.FP = defUseF
    | defUse CB.CC = defUseCC
    | defUse _ = error "defUse"

  (*========================================================================
   *  Annotations 
   *========================================================================*)
  fun getAnnotations(I.ANNOTATION{i,a}) = 
       let val (i,an) = getAnnotations i in (i,a::an) end
    | getAnnotations i = (i,[])
  fun annotate(i,a) = I.ANNOTATION{i=i,a=a}

  (*========================================================================
   *  Replicate an instruction
   *========================================================================*)
  fun replicate(I.ANNOTATION{i,a}) = I.ANNOTATION{i=replicate i,a=a}
    | replicate(I.INSTR(I.COPY{tmp=SOME _, dst, src, impl})) =  
        I.copy{tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src, impl=ref NONE}
    | replicate(I.INSTR(I.FCOPY{tmp=SOME _, dst, src, impl})) = 
        I.fcopy{tmp=SOME(I.FDirect(C.newFreg())), 
                dst=dst, src=src, impl=ref NONE}
    | replicate i = i
end


