functor PPCProps
  (structure PPCInstr : PPCINSTR
   structure Shuffle : PPCSHUFFLE 
      where I = PPCInstr
  ) : INSN_PROPERTIES = 
struct
  structure I = PPCInstr
  structure C = I.C
  structure LE = LabelExp

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.impossible ("PPCProps."^msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  fun instrKind(I.BC _) = IK_JUMP
    | instrKind(I.BCLR _) = IK_JUMP
    | instrKind(I.B _) = IK_JUMP
    | instrKind(I.ARITHI{oper=I.OR, rt=0, ra=0, im=I.ImmedOp 0}) = IK_NOP
    | instrKind _ = IK_INSTR

  fun moveInstr(I.COPY _) = true
    | moveInstr(I.FCOPY _) = true
    | moveInstr  _ = false

  fun nop () = I.ARITHI{oper=I.OR, rt=0, ra=0, im=I.ImmedOp 0}

  fun moveTmpR(I.COPY{tmp as SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.FCOPY{tmp as SOME(I.FDirect f), ...}) = SOME f
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.FCOPY{src, dst, ...}) = (dst, src)
    | moveDstSrc _ = error "moveDstSrc"

  fun copy {src, dst} = 
    I.COPY{src=src, dst=dst, impl=ref NONE, 
	   tmp=case src of [_] => NONE | _ => SOME(I.Direct(C.newReg()))}

  fun fcopy{src, dst} = let
    val trans = map (fn r => if r >= 32 andalso r < 64 then r-32 else r)
  in
    I.FCOPY{src=trans src, dst=trans dst, impl=ref NONE, 
	    tmp=case src of [_] => NONE | _ => SOME(I.FDirect(C.newFreg()))}
  end

  fun splitCopies{regmap,insns} = let
    val shuffle = Shuffle.shuffle
    val shufflefp = Shuffle.shufflefp
    fun scan([],is') = rev is'
      | scan(I.COPY{dst,src,tmp,...}::is,is') = 
	  scan(is,shuffle{regMap=regmap,src=src,dst=dst,temp=tmp}@is')
      | scan(I.FCOPY{dst,src,tmp,...}::is,is') = 
	  scan(is,shufflefp{regMap=regmap,src=src,dst=dst,temp=tmp}@is')
      | scan(i::is,is') = scan(is,i::is')
  in scan(insns,[]) 
  end

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
    | branchTargets _ = error "branchTargets"

  fun jump lab = I.B{addr=I.LabelOp(LE.LABEL lab), LK=false}

  fun setTargets _ = error " setTargets"

  fun negateConditional _ = error "negateConditional"

  fun defUseR instr = let
    fun operand (I.RegOp r) = [r]
      | operand _ = []
  in
    case instr
    of I.L{sz, rt, ra, d, ...} =>
       (case sz
	 of (I.Byte | I.Half | I.Word) => ([rt], ra::operand d)
          | (I.Single | I.Double) => ([], ra::operand d)
        (*esac*))
     | I.ST{sz, rs, ra, d, ...} => 
       (case sz
	 of (I.Byte | I.Half | I.Word) => ([], rs::ra::operand d)
          | (I.Single | I.Double) => ([], ra::operand d)
        (*esac*))
     | I.UNARY{rt, ra, ...} => ([rt], [ra])
     | I.ARITH{rt, ra, rb, ...} => ([rt], [ra,rb])
     | I.ARITHI{rt, ra, im, ...} => ([rt], ra::operand im)
     | I.ROTATE{ra, rs, sh, ...} => ([ra], rs::operand sh)
     | I.COMPARE{ra, rb, ...} => ([], ra::operand rb)
     | I.MTSPR{rs, ...} => ([], [rs])
     | I.MFSPR{rt, ...} => ([rt], [])
     | I.TWI{to, ra, si} => ([], [ra])
     | I.CALL{def, use} => (#1 def, #1 use)
     | I.COPY{dst, src, tmp, ...} => 
        (case tmp
	  of NONE => (dst, src)
	   | SOME(I.Direct r) => (r::dst, src)
	(* | SOME(I.Displace{base, disp}) => (dst, base::src) *)
	 (*esac*))
     | _ => ([], [])
  end

  fun defUseF instr = 
   (case instr
    of I.L{sz=(I.Single|I.Double), rt, ...} => ([rt],[])
     | I.ST{sz=(I.Single|I.Double), rs, ...} => ([], [rs])
     | I.FCOMPARE{fa, fb, ...}  => ([], [fa, fb])
     | I.FUNARY{ft, fb, ...}  => ([ft], [fb])
     | I.FARITH{ft, fa, fb, ...}  => ([ft], [fa, fb])
     | I.CALL{def, use} => (#2 def, #2 use)
     | I.FCOPY{dst, src, tmp, ...} => 
        (case tmp
	  of SOME(I.FDirect f) => (f::dst, src)
	   | _ => (dst, src)
	 (*esac*))
     | _ => ([], [])
    (*esac*))

  fun defUseCC instr = error "defUseCC: not implemented"

  fun defUse C.GP = defUseR
    | defUse C.FP = defUseF
    | defUse C.CC = defUseCC
    | defUse _ = error "defUse"
end


