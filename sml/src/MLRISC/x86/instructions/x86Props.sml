(* x86Props.sml -- 32bit, x86 instruction semantic properties
 *
 * COPYRIGHT (c) 1997 Bell Laboratories.
 *)

functor X86Props(X86Instr : X86INSTR) : INSN_PROPERTIES =
struct
  structure I = X86Instr
  structure C = I.C
  structure LE = LabelExp

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("X86Props",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL | IK_GROUP
                | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES
 (*========================================================================
  *  Instruction Kinds
  *========================================================================*)
  fun instrKind (I.JMP _) = IK_JUMP
    | instrKind (I.JCC _) = IK_JUMP
    | instrKind (I.COPY _) = IK_COPY
    | instrKind (I.FCOPY _) = IK_COPY
    | instrKind (I.CALL _) = IK_CALL
    | instrKind (I.ANNOTATION{i,...}) = instrKind i
    | instrKind (I.GROUP _) = IK_GROUP
    | instrKind _ = IK_INSTR


  fun moveInstr(I.COPY _) = true
    | moveInstr(I.FCOPY _) = true
    | moveInstr(I.MOVE{mvOp=I.MOVL, src=I.Direct _, dst=I.MemReg _, ...}) = true
    | moveInstr(I.MOVE{mvOp=I.MOVL, src=I.MemReg _, dst=I.Direct _, ...}) = true
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr _ = false 

  val nop = fn () => I.NOP


 (*========================================================================
  *  Parallel Move
  *========================================================================*)
  fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.FCOPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.MOVE{src=I.Direct rs, dst=I.MemReg rd, ...}) = ([rd], [rs])
    | moveDstSrc(I.MOVE{src=I.MemReg rs, dst=I.Direct rd, ...}) = ([rd], [rs])
    | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc _ = error "moveDstSrc"

 (*=====================================================================
  *  Branches and Calls/Returns
  *=====================================================================*)
  fun branchTargets(I.JMP(_, [])) = [ESCAPES]
    | branchTargets(I.JMP(_, labs)) = map LABELLED labs
    | branchTargets(I.RET _) = [ESCAPES]
    | branchTargets(I.JCC{opnd=I.ImmedLabel(LE.LABEL(lab)), ...}) = 
        [FALLTHROUGH, LABELLED lab]
    | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets _ = error "branchTargets"

  fun jump label = I.JMP (I.ImmedLabel(LE.LABEL label), [label])

  exception NotImplemented
  fun setTargets(I.JMP(I.ImmedLabel _,_),[l]) = jump l
    | setTargets(I.JMP(opnd,_),_) = error "setTargets"
    | setTargets(I.JCC{cond,opnd=I.ImmedLabel _},[f,t]) =
        I.JCC{cond=cond,opnd=I.ImmedLabel(LE.LABEL t)}
    | setTargets(I.JCC _,_) = error "setTargets"
    | setTargets(I.ANNOTATION{i,a},l) = I.ANNOTATION{i=setTargets(i,l),a=a}
    | setTargets(i,_) = i
  fun negateConditional _ = raise NotImplemented

  val immedRange={lo= ~1073741824, hi=1073741823}
  val toInt32 = Int32.fromLarge o Int.toLarge
  fun loadImmed{immed,t} =
      I.MOVE{mvOp=I.MOVL,src=I.Immed(toInt32 immed),dst=I.Direct t}

 (*=====================================================================
  *  Hashing and Equality on operands
  *=====================================================================*)
   fun hashOpn(I.Immed i) = Word.fromInt(Int32.toInt i)
     | hashOpn(I.Const c) = I.Constant.hash c
     | hashOpn(I.ImmedLabel le) = LabelExp.hash le + 0w123
     | hashOpn(I.Relative i) = Word.fromInt i + 0w1232
     | hashOpn(I.LabelEA le) = LabelExp.hash le + 0w44444
     | hashOpn(I.Direct r)  = Word.fromInt r
     | hashOpn(I.MemReg r)  = Word.fromInt r + 0w2123
     | hashOpn(I.FDirect f) = Word.fromInt f + 0w8888
     | hashOpn(I.Displace {base, disp, ...}) = hashOpn disp + Word.fromInt base
     | hashOpn(I.Indexed {base, index, scale, disp, ...}) =
         Word.fromInt index + Word.fromInt scale + hashOpn disp
   fun eqOpn(I.Immed a,I.Immed b) = a = b
     | eqOpn(I.Const a,I.Const b) = I.Constant.==(a,b)
     | eqOpn(I.ImmedLabel a,I.ImmedLabel b) = LabelExp.==(a,b)
     | eqOpn(I.Relative a,I.Relative b) = a = b
     | eqOpn(I.LabelEA a,I.LabelEA b) = LabelExp.==(a,b)
     | eqOpn(I.Direct a,I.Direct b) = a = b
     | eqOpn(I.MemReg a,I.MemReg b) = a = b
     | eqOpn(I.FDirect a,I.FDirect b) = a = b
     | eqOpn(I.Displace{base=a,disp=b,...},I.Displace{base=c,disp=d,...}) =
          a = c andalso eqOpn(b,d)
     | eqOpn(I.Indexed{base=a,index=b,scale=c,disp=d,...},
             I.Indexed{base=e,index=f,scale=g,disp=h,...}) =
          b = f andalso c = g andalso a = e andalso eqOpn(d,h)
     | eqOpn _ = false

 (*========================================================================
  *  Definition and use (for register allocation mainly)
  *========================================================================*)
  val eaxPair = [C.edx, C.eax]

  fun defUseR instr = let
    fun operandAcc(I.Direct r, acc) = r::acc
      | operandAcc(I.MemReg r, acc) = r::acc
      | operandAcc(I.Displace{base, ...}, acc) = base::acc
      | operandAcc(I.Indexed{base=SOME b, index, ...}, acc) = b::index::acc
      | operandAcc(I.Indexed{base=NONE, index, ...}, acc) = index::acc
      | operandAcc(_, acc) = acc

    fun operandUse opnd = operandAcc(opnd, [])

    fun operandDef(I.Direct r) = [r]
      | operandDef(I.MemReg r) = [r]
      | operandDef _ = []

    fun multdiv{src, multDivOp} = let
      val uses = operandUse src
    in
      case multDivOp
       of (I.IDIV | I.UDIV) => (eaxPair, C.edx::C.eax::uses)
        | I.UMUL => (eaxPair, C.eax::uses)
    end

    fun unary {unOp, opnd} = (operandDef opnd, operandUse opnd)
  in
    case instr
     of I.JMP(opnd, _)        => ([], operandUse opnd)
      | I.JCC{opnd, ...}      => ([], operandUse opnd)
      | I.CALL(opnd,defs,uses,_)=> (#1 defs, operandAcc(opnd, #1 uses))
      | I.MOVE{src, dst=I.Direct r, ...} => ([r], operandUse src)
      | I.MOVE{src, dst=I.MemReg r, ...} => ([r], operandUse src)
      | I.MOVE{src, dst, ...} => ([], operandAcc(dst, operandUse src))
      | I.LEA{r32, addr}      => ([r32], operandUse addr)
      | I.CMP{lsrc, rsrc}     => ([], operandAcc(lsrc, operandUse rsrc))
      | I.BINARY{src,dst,...} => (operandDef dst, operandAcc(src, operandUse dst))
      | I.MULTDIV arg	      => multdiv arg
      | I.MUL3{src1, src2=SOME _, dst}=> ([dst], operandUse src1)
      | I.MUL3{src1, dst, ...}=> ([dst], dst::operandUse src1)

      | I.UNARY arg	      => unary arg
      | I.PUSH arg	      => ([], operandUse arg)
      | I.POP arg	      => (operandDef arg, [])
      | I.CDQ		      => ([C.edx], [C.eax])

      | I.COPY{dst, src, tmp=SOME(I.Direct r), ...}   => (r::dst, src)
      | I.COPY{dst, src, ...} => (dst, src)
      | I.FSTP opnd	      => ([], operandUse opnd)
      | I.FLD opnd	      => ([], operandUse opnd)
      | I.FILD opnd           => ([], operandUse opnd)
      | I.FBINARY{src, ...}   => ([], operandUse src)
      | I.FNSTSW	      => ([C.eax], [])
      | I.SAHF		      => ([], [C.eax])
      | I.ANNOTATION{a, i, ...} => defUseR i
      | _		      => ([], [])
  end (* defUseR *)

  fun defUseF instr = let
    fun operand(I.FDirect f) = [f]
      | operand _ = []
  in
    case instr
     of I.FSTP opnd		=> (operand opnd, [])
      | I.FLD opnd		=> ([], operand opnd)
      | I.CALL(_, defs, uses,_)	=> (#2 defs, #2 uses)
      | I.FBINARY{dst, src, ...}=> (operand dst, operand dst @ operand src)
      | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...}  => (f::dst, src)
      | I.FCOPY{dst, src, ...}  => (dst, src)
      | I.ANNOTATION{a, i, ...} => defUseF i
      | _  => ([], [])
  end

  fun defUse C.GP = defUseR
    | defUse C.FP = defUseF
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

