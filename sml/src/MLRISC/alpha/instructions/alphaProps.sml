(* alphaProps.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor AlphaProps(AlphaInstr:ALPHAINSTR):INSN_PROPERTIES =
struct
    structure I = AlphaInstr
    structure C = I.C
    structure LE = LabelExp

    exception NegateConditional

    fun error msg = MLRiscErrorMsg.impossible ("alphaProps."^msg)

    val zeroR = 31

    datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL | IK_GROUP
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
      | instrKind(I.RET _)     = IK_JUMP
      | instrKind(I.ANNOTATION{i,...}) = instrKind i
      | instrKind(I.GROUP _)   = IK_GROUP
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
    fun branchTargets(I.BRANCH(I.BR, _, lab)) = [LABELLED lab]
      | branchTargets(I.BRANCH(_, _, lab))  = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.FBRANCH(_, _, lab)) = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.JMPL(_,[]))       = [ESCAPES]
      | branchTargets(I.JMPL(_,labs))     = map LABELLED labs
      | branchTargets(I.RET _)            = [ESCAPES]
      | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
      | branchTargets _ = error "branchTargets"

    fun jump label = I.BRANCH(I.BR,31,label)

    val immedRange = {lo= ~32768, hi = 32768}
    fun loadImmed{immed,t} = I.LDA{r=t,b=31,d=I.IMMop immed}

    fun setTargets(I.BRANCH(I.BR,0,_),[L]) = I.BRANCH(I.BR,0,L)
      | setTargets(I.BRANCH(b,r,_),[F,T])  = I.BRANCH(b,r,T)
      | setTargets(I.FBRANCH(b,r,_),[F,T]) = I.FBRANCH(b,r,T)
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
      of I.BRANCH(br,r,label) => I.BRANCH(revBranch br,r,label)
       | I.FBRANCH(br,r,label) => I.FBRANCH(revFBranch br,r,label)
       | I.ANNOTATION{i,a} => I.ANNOTATION{i=negateConditional i,a=a}
       | _ => raise NegateConditional
    end

   (*========================================================================
    *  Equality and hashing for operands
    *========================================================================*)
   fun hashOpn(I.REGop r) = Word.fromInt r
     | hashOpn(I.IMMop i) = Word.fromInt i
     | hashOpn(I.HILABop l) = LabelExp.hash l
     | hashOpn(I.LOLABop l) = LabelExp.hash l
     | hashOpn(I.LABop l) = LabelExp.hash l
     | hashOpn(I.CONSTop c) = I.Constant.hash c

   fun eqOpn(I.REGop a,I.REGop b) = a = b
     | eqOpn(I.IMMop a,I.IMMop b) = a = b
     | eqOpn(I.HILABop a,I.HILABop b) = LabelExp.==(a,b)
     | eqOpn(I.LOLABop a,I.LOLABop b) = LabelExp.==(a,b)
     | eqOpn(I.LABop a,I.LABop b) = LabelExp.==(a,b)
     | eqOpn(I.CONSTop a,I.CONSTop b) = I.Constant.==(a,b)
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
	 | I.JSR({r, b, ...}, def, use, mem) => (r:: #1 def, b:: #1 use)
	 | I.RET{r, b, ...} => ([r],[b])
	 | I.BRANCH(I.BR, reg, _) => ([reg], [])
	 | I.BRANCH(_, reg, _) => ([], [reg])
	 (* operate *)
	 | I.OPERATE arg => Oper arg
	 | I.PSEUDOARITH {oper, ra, rb=I.REGop rb, rc, tmps} => 
	     (rc:: #1 tmps, [ra, rb])
	 | I.PSEUDOARITH {oper, ra, rb, rc, tmps} => (rc:: #1 tmps, [ra])
	 | I.OPERATEV arg => trap(Oper arg)
	 | I.CMOVE{ra,rb,rc,...} => ([rc],Opn(rb,[ra,rc]))
	 (* copy *)
	 | I.COPY{dst, src, tmp=SOME(I.Direct r), ...} => (r::dst, src)
	 | I.COPY{dst, src, ...} => (dst, src)
	 (* floating operate *)
	 | I.FOPERATEV _ => trap([], [])
	 | I.TRAPB 	=> trap([],[])
	 (* macro *)
	 | I.CALL_PAL{def,use, ...} => (def, use)
         | I.ANNOTATION{a, i, ...} => defUseR i
	 | _  		=> ([],[])
      end

    (* Use of FP registers *)
    fun defUseF instr =
      case instr of
	I.DEFFREG freg				=> ([freg], [])
      | I.FBRANCH(_, freg, lab)			=>  ([],[freg])
      | I.FLOAD{r, ...}				=> ([r], [])
      | I.FSTORE{r, ...}			=> ([], [r])
      | I.FOPERATE{fa, fb, fc, ...}		=> ([fc], [fa, fb])
      | I.FUNARY{fb, fc, ...}		        => ([fc], [fb])
      | I.PSEUDOARITH{tmps, ...}		=> (#2 tmps, [])
      | I.FOPERATEV{fa, fb, fc, ...}		=> ([fc], [fa, fb, fc])
      | I.FCMOVE{fa,fb,fc,...}                  => ([fc], [fa, fb, fc])
      | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...} => (f::dst, src)
      | I.FCOPY{dst, src, ...}			=> (dst, src) 
      | I.JSR(_,def,use, mem)	     => (#2 def,#2 use)
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

    (*=======================================================================
     *  Groups 
     *=======================================================================*)
    fun getGroup(I.ANNOTATION{i,...}) = getGroup i
      | getGroup(I.GROUP r) = r
      | getGroup _ = error "getGroup"

    val makeGroup = I.GROUP
end


