(* x86Props.sml -- 32bit, x86 instruction semantic properties
 *
 * COPYRIGHT (c) 1997 Bell Laboratories.
 *)

functor X86Props(X86Instr : X86INSTR) : INSN_PROPERTIES =
struct
  structure I = X86Instr
  structure C = I.C
  structure LE = I.LabelExp

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("X86Props",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
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
    | instrKind (I.PHI _)    = IK_PHI
    | instrKind (I.SOURCE _) = IK_SOURCE
    | instrKind (I.SINK _)   = IK_SINK
    | instrKind (I.RET _) = IK_JUMP
    | instrKind (I.ANNOTATION{i,...}) = instrKind i
    | instrKind _ = IK_INSTR


  fun moveInstr(I.COPY _) = true
    | moveInstr(I.FCOPY _) = true
    | moveInstr(I.MOVE{mvOp=I.MOVL, src=I.Direct _, dst=I.MemReg _, ...}) = true
    | moveInstr(I.MOVE{mvOp=I.MOVL, src=I.MemReg _, dst=I.Direct _, ...}) = true
    | moveInstr(I.FMOVE{fsize=I.FP64,src=I.FPR _,dst=I.FPR _, ...}) = true
    | moveInstr(I.FMOVE{fsize=I.FP64,src=I.FPR _,dst=I.FDirect _, ...}) = true
    | moveInstr(I.FMOVE{fsize=I.FP64,src=I.FDirect _,dst=I.FPR _, ...}) = true
    | moveInstr(I.FMOVE{fsize=I.FP64,src=I.FDirect _,dst=I.FDirect _, ...}) = true
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr _ = false 

  val nop = fn () => I.NOP


 (*========================================================================
  *  Parallel Move
  *========================================================================*)
  fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
    | moveTmpR(I.FCOPY{tmp=SOME(I.FPR f), ...}) = SOME f
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.FCOPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.MOVE{src=I.Direct rs, dst=I.MemReg rd, ...}) = ([rd], [rs])
    | moveDstSrc(I.MOVE{src=I.MemReg rs, dst=I.Direct rd, ...}) = ([rd], [rs])
    | moveDstSrc(I.FMOVE{src=I.FPR rs, dst=I.FPR rd, ...}) = ([rd], [rs])
    | moveDstSrc(I.FMOVE{src=I.FDirect rs, dst=I.FPR rd, ...}) = ([rd], [rs])
    | moveDstSrc(I.FMOVE{src=I.FPR rs, dst=I.FDirect rd, ...}) = ([rd], [rs])
    | moveDstSrc(I.FMOVE{src=I.FDirect rs, dst=I.FDirect rd, ...}) = ([rd], [rs])
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
  fun loadOperand{opn,t} = I.MOVE{mvOp=I.MOVL,src=opn,dst=I.Direct t}

 (*=====================================================================
  *  Hashing and Equality on operands
  *=====================================================================*)
   fun hashOpn(I.Immed i) = Word.fromInt(Int32.toInt i)
     | hashOpn(I.ImmedLabel le) = LE.hash le + 0w123
     | hashOpn(I.Relative i) = Word.fromInt i + 0w1232
     | hashOpn(I.LabelEA le) = LE.hash le + 0w44444
     | hashOpn(I.Direct r)  = Word.fromInt r
     | hashOpn(I.MemReg r)  = Word.fromInt r + 0w2123
     | hashOpn(I.ST f) = Word.fromInt f + 0w88
     | hashOpn(I.FPR f) = Word.fromInt f + 0w881
     | hashOpn(I.FDirect f) = Word.fromInt f + 0w31245
     | hashOpn(I.Displace {base, disp, ...}) = 
         hashOpn disp + Word.fromInt base
     | hashOpn(I.Indexed {base, index, scale, disp, ...}) =
         Word.fromInt index + Word.fromInt scale + hashOpn disp
   fun eqOpn(I.Immed a,I.Immed b) = a = b
     | eqOpn(I.ImmedLabel a,I.ImmedLabel b) = LE.==(a,b)
     | eqOpn(I.Relative a,I.Relative b) = a = b
     | eqOpn(I.LabelEA a,I.LabelEA b) = LE.==(a,b)
     | eqOpn(I.Direct a,I.Direct b) = a = b
     | eqOpn(I.MemReg a,I.MemReg b) = a = b
     | eqOpn(I.FDirect a,I.FDirect b) = a = b
     | eqOpn(I.ST a,I.ST b) = a = b
     | eqOpn(I.FPR a,I.FPR b) = a = b
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

    fun operandUse2(src1, src2) = ([], operandAcc(src1, operandUse src2))
    fun operandUse3(x, y, z) = ([], operandAcc(x, operandAcc(y, operandUse y)))

    fun operandDef(I.Direct r) = [r]
      | operandDef(I.MemReg r) = [r]
      | operandDef _ = []

    fun multdiv{src, multDivOp} = let
      val uses = operandUse src
    in
      case multDivOp
       of (I.IDIVL | I.DIVL) => (eaxPair, C.edx::C.eax::uses)
        | I.MULL => (eaxPair, C.eax::uses)
    end

    fun unary opnd = (operandDef opnd, operandUse opnd)
    fun cmptest{lsrc, rsrc} = ([], operandAcc(lsrc, operandUse rsrc))
    fun push arg = ([C.stackptrR], operandAcc(arg, [C.stackptrR]))
    fun float opnd = ([], operandUse opnd)
  in
    case instr
     of I.JMP(opnd, _)        => ([], operandUse opnd)
      | I.JCC{opnd, ...}      => ([], operandUse opnd)
      | I.CALL(opnd,defs,uses,_)=> (#1 defs, operandAcc(opnd, #1 uses))
      | I.MOVE{src, dst=I.Direct r, ...} => ([r], operandUse src)
      | I.MOVE{src, dst=I.MemReg r, ...} => ([r], operandUse src)
      | I.MOVE{src, dst, ...} => ([], operandAcc(dst, operandUse src))
      | I.LEA{r32, addr}      => ([r32], operandUse addr)
      | ( I.CMPL arg | I.CMPW arg | I.CMPB arg
        | I.TESTL arg | I.TESTW arg | I.TESTB arg ) => cmptest arg 
      | I.BITOP{lsrc, rsrc, ...} => cmptest{lsrc=lsrc,rsrc=rsrc}
      | I.BINARY{binOp=I.XORL,src=I.Direct rs,dst=I.Direct rd,...} =>   
           if rs=rd then ([rd],[]) else ([rd],[rs,rd])
      | I.BINARY{src,dst,...} =>   
           (operandDef dst, operandAcc(src, operandUse dst))
      | I.ENTER _             => ([C.esp, C.ebp], [C.esp, C.ebp])
      | I.LEAVE               => ([C.esp, C.ebp], [C.esp, C.ebp])
      | I.MULTDIV arg	      => multdiv arg
      | I.MUL3{src1, src2=SOME _, dst}=> ([dst], operandUse src1)
      | I.MUL3{src1, dst, ...}=> ([dst], dst::operandUse src1)

      | I.UNARY{opnd, ...}    => unary opnd
      | I.SET{opnd, ...}      => unary opnd
      | ( I.PUSHL arg | I.PUSHW arg | I.PUSHB arg ) => push arg
      | I.POP arg	      => (C.stackptrR::operandDef arg, [C.stackptrR])
      | I.CDQ		      => ([C.edx], [C.eax])

      | I.COPY{dst, src, tmp=SOME(I.Direct r), ...}   => (r::dst, src)
      | I.COPY{dst, src, ...} => (dst, src)
      | I.FSTPT opnd	      => float opnd
      | I.FSTPL opnd	      => float opnd
      | I.FSTPS opnd	      => float opnd 
      | I.FSTL opnd	      => float opnd
      | I.FSTS opnd	      => float opnd 
      | I.FLDL opnd	      => float opnd
      | I.FLDS opnd	      => float opnd
      | I.FILD opnd           => float opnd
      | I.FILDL opnd          => float opnd
      | I.FILDLL opnd         => float opnd
      | I.FBINARY{src, ...}   => ([], operandUse src)
      | I.FIBINARY{src, ...}  => ([], operandUse src)
      | I.FENV{opnd, ...}     => ([], operandUse opnd)
      | I.FNSTSW	      => ([C.eax], [])
      | I.FUCOM opnd          => float opnd
      | I.FUCOMP opnd         => float opnd

      | I.FMOVE{src, dst, ...} => operandUse2(src, dst) 
      | I.FILOAD{ea, dst, ...} => operandUse2(ea, dst) 
      | I.FCMP{lsrc, rsrc, ...} => operandUse2(lsrc, rsrc)
      | I.FBINOP{lsrc, rsrc, dst, ...} => operandUse3(lsrc, rsrc, dst)
      | I.FIBINOP{lsrc, rsrc, dst, ...} => operandUse3(lsrc, rsrc, dst)
      | I.FUNOP{src, dst, ...} => operandUse2(src, dst)

      | I.SAHF		      => ([], [C.eax])
        (* This sets the low order byte, 
         * do potentially it may define *and* use 
         *)
      | I.CMOV{src,dst,...} => ([dst], operandAcc(src, [dst]))
      | I.ANNOTATION{a=C.DEF_USE{cellkind=C.GP,defs,uses}, i, ...} => 
        let val (d,u) = defUseR i in (defs@d, u@uses) end
      | I.ANNOTATION{a, i, ...} => defUseR i
      | _		      => ([], [])
  end (* defUseR *)

  fun defUseF instr = let
    fun operand(I.FDirect f) = [f]
      | operand(I.FPR f) = [f]
      | operand _ = []

    fun operandAcc(I.FDirect f, acc) = f::acc
      | operandAcc(I.FPR f, acc) = f::acc
      | operandAcc(_ , acc) = acc

    fun fbinop(lsrc, rsrc, dst) = 
    let val def = operand dst
        val use = operandAcc(lsrc, operand rsrc)
    in  (def, use) 
    end

    val fcmpTmp = [C.ST 0]

  in
    case instr
     of I.FSTPT opnd            => (operand opnd, [])  
      | I.FSTPL opnd		=> (operand opnd, [])
      | I.FSTPS opnd		=> (operand opnd, [])
      | I.FSTL opnd		=> (operand opnd, [])
      | I.FSTS opnd		=> (operand opnd, [])
      | I.FLDT opnd		=> ([], operand opnd)
      | I.FLDL opnd		=> ([], operand opnd)
      | I.FLDS opnd		=> ([], operand opnd)
      | I.FUCOM opnd            => ([], operand opnd)
      | I.FUCOMP opnd           => ([], operand opnd)
      | I.CALL(_, defs, uses,_)	=> (#2 defs, #2 uses)
      | I.FBINARY{dst, src, ...}=> (operand dst, operand dst @ operand src)
      | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...}  => (f::dst, src)
      | I.FCOPY{dst, src, tmp=SOME(I.FPR f), ...}  => (f::dst, src)
      | I.FCOPY{dst, src, ...}  => (dst, src)

      | I.FMOVE{src, dst, ...} => (operand dst, operand src) 
      | I.FILOAD{ea, dst, ...} => (operand dst, []) 
      | I.FCMP{lsrc, rsrc, ...} => (fcmpTmp, operandAcc(lsrc, operand rsrc))
      | I.FBINOP{lsrc, rsrc, dst, ...} => fbinop(lsrc, rsrc, dst)
      | I.FIBINOP{lsrc, rsrc, dst, ...} => fbinop(lsrc, rsrc, dst)
      | I.FUNOP{src, dst, ...} => (operand dst, operand src)

      | I.ANNOTATION{a=C.DEF_USE{cellkind=C.FP,defs,uses}, i, ...} => 
        let val (d,u) = defUseF i in (defs@d, u@uses) end
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
   *  Replicate an instruction
   *========================================================================*)
  fun replicate(I.ANNOTATION{i,a}) = I.ANNOTATION{i=replicate i,a=a}
    | replicate(I.COPY{tmp=SOME _, dst, src}) =  
        I.COPY{tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src}
    | replicate(I.FCOPY{tmp=SOME _, dst, src}) = 
        I.FCOPY{tmp=SOME(I.FDirect(C.newFreg())), dst=dst, src=src}
    | replicate i = i
end

