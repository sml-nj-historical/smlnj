(* alphaProps.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor AlphaProps(AlphaInstr:ALPHAINSTR):INSN_PROPERTIES =
struct
    structure I = AlphaInstr
    structure C = I.C
    structure LE = I.LabelExp

    exception NegateConditional

    fun error msg = MLRiscErrorMsg.impossible ("alphaProps."^msg)

    val zeroR = Option.valOf(C.zeroReg C.GP)

    datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                  | IK_PHI | IK_SOURCE | IK_SINK
    datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   (*========================================================================
    *  Instruction Kinds
    *========================================================================*)
    fun instrKind(I.BRANCH _)  = IK_JUMP
      | instrKind(I.FBRANCH _) = IK_JUMP
      | instrKind(I.JMPL _)    = IK_JUMP
      | instrKind(I.COPY _)    = IK_COPY
      | instrKind(I.FCOPY _)   = IK_COPY
      | instrKind(I.JSR _)     = IK_CALL
      | instrKind(I.BSR _)     = IK_CALL
      | instrKind(I.RET _)     = IK_JUMP
      | instrKind(I.PHI _)     = IK_PHI
      | instrKind(I.SOURCE _)  = IK_SOURCE
      | instrKind(I.SINK _)    = IK_SINK
      | instrKind(I.ANNOTATION{i,...}) = instrKind i
      | instrKind _            = IK_INSTR

    fun moveInstr(I.COPY _)  = true
      | moveInstr(I.FCOPY _) = true
      | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
      | moveInstr _	     = false

    val nop = 
      fn () => I.OPERATE{oper=I.BIS, ra=zeroR, rb=I.REGop zeroR, rc=zeroR}

   (*========================================================================
    *  Parallel Move
    *========================================================================*)
    fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
      | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
      | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
      | moveTmpR _ = NONE

    fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
      | moveDstSrc(I.FCOPY{dst, src, ...}) = (dst, src)
      | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
      | moveDstSrc _ = error "moveDstSrc"

   (*========================================================================
    *  Branches and Calls/Returns
    *========================================================================*)
    fun branchTargets(I.BRANCH{b=I.BR, lab, ...}) = [LABELLED lab]
      | branchTargets(I.BRANCH{lab, ...})  = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.FBRANCH{lab, ...}) = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.JMPL(_,[]))       = [ESCAPES]
      | branchTargets(I.JMPL(_,labs))     = map LABELLED labs
      | branchTargets(I.RET _)            = [ESCAPES]
      | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
      | branchTargets _ = error "branchTargets"

    fun jump label = I.BRANCH{b=I.BR,r=zeroR,lab=label}

    val immedRange = {lo= ~32768, hi = 32768}
    fun loadImmed{immed,t} = 
        I.LDA{r=t,b=zeroR,
              d=if #lo immedRange <= immed andalso immed <= #hi immedRange
              then I.IMMop immed else I.LABop(LE.INT immed)}
    fun loadOperand{opn,t} = I.LDA{r=t,b=zeroR,d=opn}

    fun setTargets(I.BRANCH{b=I.BR,r as C.CELL{id=31,...}, ...},[L]) = 
             I.BRANCH{b=I.BR,r=r,lab=L}
      | setTargets(I.BRANCH{b,r,...},[F,T])  = I.BRANCH{b=b,r=r,lab=T}
      | setTargets(I.FBRANCH{b,f,...},[F,T]) = I.FBRANCH{b=b,f=f,lab=T}
      | setTargets(I.JMPL(x,_),labs)       = I.JMPL(x,labs)
      | setTargets(I.ANNOTATION{i,a},labs) = 
            I.ANNOTATION{i=setTargets(i,labs),a=a}
      | setTargets(i,_) = i

    fun negateConditional br = let
      fun revBranch I.BEQ  = I.BNE 
	| revBranch I.BGE  = I.BLT 
	| revBranch I.BGT  = I.BLE 
	| revBranch I.BLE  = I.BGT 
	| revBranch I.BLT  = I.BGE 
	| revBranch I.BLBC = I.BLBS 
	| revBranch I.BLBS = I.BLBC 
	| revBranch _ = raise NegateConditional
      fun revFBranch I.FBEQ  = I.FBNE 
        | revFBranch I.FBNE  = I.FBEQ 
	| revFBranch I.FBGE  = I.FBLT 
	| revFBranch I.FBGT  = I.FBLE 
	| revFBranch I.FBLE  = I.FBGT 
	| revFBranch I.FBLT  = I.FBGE 

    in
      case br
      of I.BRANCH{b,r,lab} => I.BRANCH{b=revBranch b,r=r,lab=lab}
       | I.FBRANCH{b,f,lab} => I.FBRANCH{b=revFBranch b,f=f,lab=lab}
       | I.ANNOTATION{i,a} => I.ANNOTATION{i=negateConditional i,a=a}
       | _ => raise NegateConditional
    end

   (*========================================================================
    *  Equality and hashing for operands
    *========================================================================*)
   fun hashOpn(I.REGop r) = C.hashCell r
     | hashOpn(I.IMMop i) = Word.fromInt i
     | hashOpn(I.HILABop l) = I.LabelExp.hash l
     | hashOpn(I.LOLABop l) = I.LabelExp.hash l
     | hashOpn(I.LABop l) = I.LabelExp.hash l

   fun eqOpn(I.REGop a,I.REGop b) = C.sameColor(a,b)
     | eqOpn(I.IMMop a,I.IMMop b) = a = b
     | eqOpn(I.HILABop a,I.HILABop b) = I.LabelExp.==(a,b)
     | eqOpn(I.LOLABop a,I.LOLABop b) = I.LabelExp.==(a,b)
     | eqOpn(I.LABop a,I.LABop b) = I.LabelExp.==(a,b)
     | eqOpn _ = false

   (*========================================================================
    *  Definition and use (for register allocation mainly)
    *========================================================================*)
    fun defUseR instr =
      let
	fun Oper {oper, ra, rb=I.REGop rb, rc} = ([rc], [ra, rb])
	  | Oper {oper, ra, rb, rc} = ([rc], [ra])
        fun Opn(I.REGop rb,rs) = rb::rs
          | Opn(_,rs) = rs
	fun FMem (freg, (rd, _)) = ([], [rd])
	fun trap (def,use) =(def, use)
      in
	case instr of
	  (* load/store instructions *)
	   I.LDA{r, b, ...} => ([r], [b])
	 | I.LDAH{r, b, ...} => ([r], [b])
	 | I.LOAD{r, b, ...} => ([r], [b])
         | I.STORE{r, b, ...} => ([], [r,b])
	 | I.FLOAD{b, ...} => ([], [b])
	 | I.FSTORE{b, ...} => ([], [b])
	 (* branch instructions *)
	 | I.JMPL ({r, b, ...},_) => ([r], [b])
	 | I.JSR{r, b, defs, uses, ...} => (r::C.getReg defs, b::C.getReg uses)
	 | I.BSR{r, defs, uses, ...} => (r::C.getReg defs,C.getReg uses)
	 | I.RET{r, b, ...} => ([r],[b])
	 | I.BRANCH{b=I.BR, r, ...} => ([r], [])
	 | I.BRANCH{r, ...} => ([], [r])
	 (* operate *)
	 | I.OPERATE arg => Oper arg
	 | I.PSEUDOARITH {oper, ra, rb=I.REGop rb, rc, tmps} => 
	     (rc:: C.getReg tmps, [ra, rb])
	 | I.PSEUDOARITH {oper, ra, rb, rc, tmps} => (rc:: C.getReg tmps, [ra])
	 | I.OPERATEV arg => trap(Oper arg)
	 | I.CMOVE{ra,rb,rc,...} => ([rc],Opn(rb,[ra,rc]))
	 (* copy *)
	 | I.COPY{dst, src, tmp=SOME(I.Direct r), ...} => (r::dst, src)
	 | I.COPY{dst, src, ...} => (dst, src)
	 (* floating operate *)
	 | I.FOPERATEV _ => trap([], [])
	 | I.TRAPB 	=> trap([],[])
	 (* macro *)
	 | I.CALL_PAL{def,use, ...} => (C.getReg def, C.getReg use)
         | I.ANNOTATION{a=C.DEF_USE{cellkind=C.GP,defs,uses}, i, ...} => 
           let val (d,u) = defUseR i in (defs@d, u@uses) end
         | I.ANNOTATION{a, i, ...} => defUseR i
	 | _  		=> ([],[])
      end

    (* Use of FP registers *)
    fun defUseF instr =
      case instr of
	I.DEFFREG freg				=> ([freg], [])
      | I.FBRANCH{f, ...}			=>  ([],[f])
      | I.FLOAD{r, ...}				=> ([r], [])
      | I.FSTORE{r, ...}			=> ([], [r])
      | I.FOPERATE{fa, fb, fc, ...}		=> ([fc], [fa, fb])
      | I.FUNARY{fb, fc, ...}		        => ([fc], [fb])
      | I.PSEUDOARITH{tmps, ...}		=> (C.getFreg tmps, [])
      | I.FOPERATEV{fa, fb, fc, ...}		=> ([fc], [fa, fb, fc])
      | I.FCMOVE{fa,fb,fc,...}                  => ([fc], [fa, fb, fc])
      | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...} => (f::dst, src)
      | I.FCOPY{dst, src, ...}			=> (dst, src) 
      | I.JSR{defs,uses, ...}	     => (C.getFreg defs,C.getFreg uses)
      | I.BSR{defs,uses, ...}	     => (C.getFreg defs,C.getFreg uses)
      | I.ANNOTATION{a=C.DEF_USE{cellkind=C.FP,defs,uses}, i, ...} => 
        let val (d,u) = defUseF i in (defs@d, u@uses) end
      | I.ANNOTATION{a, i, ...} => defUseF i
      | _ => ([],[])

    fun defUse C.GP = defUseR
      | defUse C.FP = defUseF
      | defUse _ = error "defUse"

    (*=======================================================================
     *  Annotations 
     *=======================================================================*)
    fun getAnnotations(I.ANNOTATION{i,a}) = 
         let val (i,an) = getAnnotations i in (i,a::an) end
      | getAnnotations i = (i,[])
    fun annotate(i,a) = I.ANNOTATION{i=i,a=a}

  (*========================================================================
   *  Replicate an instruction
   *========================================================================*)
  fun replicate(I.ANNOTATION{i,a}) = I.ANNOTATION{i=replicate i,a=a}
    | replicate(I.COPY{tmp=SOME _, dst, src, impl}) =  
        I.COPY{tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src, impl=ref NONE}
    | replicate(I.FCOPY{tmp=SOME _, dst, src, impl}) = 
        I.FCOPY{tmp=SOME(I.FDirect(C.newFreg())), 
                dst=dst, src=src, impl=ref NONE}
    | replicate i = i
end
