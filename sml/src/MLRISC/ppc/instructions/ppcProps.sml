functor PPCProps(PPCInstr : PPCINSTR) : INSN_PROPERTIES = 
struct
  structure I = PPCInstr
  structure C = I.C
  structure LE = LabelExp

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("PPCProps",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL | IK_GROUP
                | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  fun instrKind(I.BC _) = IK_JUMP
    | instrKind(I.BCLR _) = IK_JUMP
    | instrKind(I.B _) = IK_JUMP
    | instrKind(I.ARITHI{oper=I.ORI, rt=0, ra=0, im=I.ImmedOp 0}) = IK_NOP
    | instrKind(I.COPY _) = IK_COPY
    | instrKind(I.FCOPY _) = IK_COPY
    | instrKind(I.CALL _) = IK_CALL
    | instrKind(I.ANNOTATION{i,...}) = instrKind i
    | instrKind(I.GROUP _) = IK_GROUP
    | instrKind _ = IK_INSTR

  fun moveInstr(I.COPY _) = true
    | moveInstr(I.FCOPY _) = true
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr  _ = false

  fun nop () = I.ARITHI{oper=I.ORI, rt=0, ra=0, im=I.ImmedOp 0}

  fun moveTmpR(I.COPY{tmp as SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.FCOPY{tmp as SOME(I.FDirect f), ...}) = SOME f
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.FCOPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc _ = error "moveDstSrc"

  fun branchTargets(I.BC{bo=I.ALWAYS, addr,  ...}) = 
      (case addr
        of I.LabelOp(LE.LABEL lab) => [LABELLED lab]
         | _ => error "branchTargets:BC:ALWAYS"
      (*esac*))
    | branchTargets(I.BC{addr, ...}) = 
      (case addr
        of I.LabelOp(LE.LABEL lab) => [LABELLED lab, FALLTHROUGH]
         | _ => error "branchTargets:BC"
      (*esac*))
    | branchTargets(I.BCLR{labels, bo=I.ALWAYS, ...}) = 
      (case labels of [] => [ESCAPES] | _ => map LABELLED labels)
    | branchTargets(I.BCLR{labels,  ...}) = 
      (case labels of [] => [ESCAPES, FALLTHROUGH] | _ => map LABELLED labels)
    | branchTargets(I.B{addr=I.LabelOp(LE.LABEL lab), LK}) = [LABELLED lab]
    | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets _ = error "branchTargets"

  fun jump lab = I.B{addr=I.LabelOp(LE.LABEL lab), LK=false}

  val immedRange = {lo= ~32768, hi=32767}

  fun loadImmed{immed,t} = 
       I.ARITHI{oper=I.ADDI, rt=t, ra=0, im=I.ImmedOp immed}

  fun setTargets _ = error " setTargets"

  fun negateConditional _ = error "negateConditional"

  fun hashOpn(I.RegOp r) = Word.fromInt r
    | hashOpn(I.ImmedOp i) = Word.fromInt i
    | hashOpn(I.LabelOp l) = LabelExp.hash l
    | hashOpn(I.ConstOp c) = I.Constant.hash c
  fun eqOpn(I.RegOp a,I.RegOp b) = a = b
    | eqOpn(I.ImmedOp a,I.ImmedOp b) = a = b
    | eqOpn(I.LabelOp a,I.LabelOp b) = LabelExp.==(a,b)
    | eqOpn(I.ConstOp a,I.ConstOp b) = I.Constant.==(a,b)
    | eqOpn _ = false

  fun defUseR instr = let
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
     | I.CALL{def, use, ...} => (#1 def, #1 use)
     | I.COPY{dst, src, tmp, ...} => 
        (case tmp
	  of NONE => (dst, src)
	   | SOME(I.Direct r) => (r::dst, src)
	(* | SOME(I.Displace{base, disp}) => (dst, base::src) *)
	 (*esac*))
     | I.ANNOTATION{a, i, ...} =>
        (case #peek BasicAnnotations.DEFUSER a of
           SOME(d,u) => (d,u)
         | NONE => defUseR i
        )
     | _ => ([], [])
  end

  fun defUseF instr = 
   (case instr
    of I.LF{ft, ...} => ([ft],[])
     | I.STF{fs, ...} => ([], [fs])
     | I.FCOMPARE{fa, fb, ...}  => ([], [fa, fb])
     | I.FUNARY{ft, fb, ...}  => ([ft], [fb])
     | I.FARITH{ft, fa, fb, ...}  => ([ft], [fa, fb])
     | I.FARITH3{ft, fa, fb, fc, ...}  => ([ft], [fa, fb, fc])
     | I.CALL{def, use, ...} => (#2 def, #2 use)
     | I.FCOPY{dst, src, tmp, ...} => 
        (case tmp
	  of SOME(I.FDirect f) => (f::dst, src)
	   | _ => (dst, src)
	 (*esac*))
     | I.ANNOTATION{a, i, ...} =>
        (case #peek BasicAnnotations.DEFUSEF a of
           SOME(d,u) => (d,u)
         | NONE => defUseF i
        )
     | _ => ([], [])
    (*esac*))

  fun defUseCC instr = error "defUseCC: not implemented"

  fun defUse C.GP = defUseR
    | defUse C.FP = defUseF
    | defUse C.CC = defUseCC
    | defUse _ = error "defUse"

  (*========================================================================
   *  Annotations 
   *========================================================================*)
  fun getAnnotations(I.ANNOTATION{i,a}) = 
       let val (i,an) = getAnnotations i in (i,a::an) end
    | getAnnotations i = (i,[])
  fun annotate(i,a) = I.ANNOTATION{i=i,a=a}

  (*========================================================================
   *  Groups 
   *========================================================================*)
  fun getGroup(I.ANNOTATION{i,...}) = getGroup i
    | getGroup(I.GROUP r) = r
    | getGroup _ = error "getGroup"

  val makeGroup = I.GROUP
end


