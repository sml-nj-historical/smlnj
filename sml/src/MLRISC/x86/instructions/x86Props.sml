(* x86Props.sml -- 32bit, x86 instruction semantic properties
 *
 * COPYRIGHT (c) 1997 Bell Laboratories.
 *)

functor X86Props
  (structure X86Instr : X86INSTR
   structure Shuffle : X86SHUFFLE
     sharing Shuffle.I = X86Instr) : INSN_PROPERTIES =
struct
  structure I = X86Instr
  structure C = I.C
  structure LE = LabelExp

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.impossible ("X86Props." ^ msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES
 (*========================================================================
  *  Instruction Kinds
  *========================================================================*)
  fun instrKind (I.JMP _) = IK_JUMP
    | instrKind (I.JCC _) = IK_JUMP
    | instrKind _ = IK_INSTR


  fun moveInstr(I.COPY _) = true
    | moveInstr(I.FCOPY _) = true
    | moveInstr _ = false 

  val nop = fn () => I.NOP


 (*========================================================================
  *  Parallel Move
  *========================================================================*)
  fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{src, dst, ...}) = (dst, src)
    | moveDstSrc(I.FCOPY{src, dst, ...}) = (dst, src)
    | moveDstSrc _ = error "moveDstSrc"

  fun copy{src, dst} =
       I.COPY{src=src, dst=dst, tmp=SOME(I.Direct(C.newReg()))} 

  fun fcopy{src, dst} = let
    fun trans r = if r >= 8 andalso r < 16 then r-8 else r
    val src = map trans src
    val dst = map trans dst
  in I.FCOPY{src=src, dst=dst, tmp=SOME(I.FDirect(C.newFreg()))}
  end
    
  fun splitCopies{regmap, insns} = let
    val shuffle = Shuffle.shuffle
    val shufflefp = Shuffle.shufflefp
    fun scan([],is') = rev is'
      | scan(I.COPY{dst,src,tmp,...}::is,is') = 
	  scan(is,shuffle{regMap=regmap,src=src,dst=dst,temp=tmp}@is')
      | scan(I.FCOPY{dst,src,tmp, ...}::is,is') = 
	  scan(is,shufflefp{regMap=regmap,src=src,dst=dst,temp=tmp}@is')
      | scan(i::is,is') = scan(is,i::is')
  in scan(insns,[]) 
  end


 (*=====================================================================
  *  Branches and Calls/Returns
  *=====================================================================*)
  fun branchTargets(I.JMP(_, [])) = [ESCAPES]
    | branchTargets(I.JMP(_, labs)) = map LABELLED labs
    | branchTargets(I.JCC{opnd=I.ImmedLabel(LE.LABEL(lab)), ...}) = 
        [FALLTHROUGH, LABELLED lab]
    | branchTargets _ = error "branchTargets"

  fun jump label = I.JMP (I.ImmedLabel(LE.LABEL label), [label])

  exception NotImplemented
  fun setTargets _ = raise NotImplemented
  fun negateConditional _ = raise NotImplemented

 (*========================================================================
  *  Definition and use (for register allocation mainly)
  *========================================================================*)
  val eaxPair = [C.edx, C.eax]

  fun defUseR instr = let
    fun operandAcc(I.Direct r, acc) = r::acc
      | operandAcc(I.Displace{base, ...}, acc) = base::acc
      | operandAcc(I.Indexed{base=SOME b, index, ...}, acc) = b::index::acc
      | operandAcc(I.Indexed{base=NONE, index, ...}, acc) = index::acc
      | operandAcc(_, acc) = acc

    fun operandUse opnd = operandAcc(opnd, [])

    fun operandDef(I.Direct r) = [r]
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
      | I.CALL(opnd,defs,uses)=> (#1 defs, operandAcc(opnd, #1 uses))
      | I.MOVE{src, dst as I.Direct _, ...} => (operandDef dst, operandUse src)
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
      | _		      => ([], [])
  end (* defUseR *)

  fun defUseF instr = let
    fun operand(I.FDirect f) = [f]
      | operand _ = []
  in
    case instr
     of I.FSTP opnd		=> (operand opnd, [])
      | I.FLD opnd		=> ([], operand opnd)
      | I.CALL(_, defs, uses)	=> (#2 defs, #2 uses)
      | I.FBINARY{dst, src, ...}=> (operand dst, operand dst @ operand src)
      | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...}  => (f::dst, src)
      | I.FCOPY{dst, src, ...}  => (dst, src)
      | _  => ([], [])
  end

  fun defUse C.GP = defUseR
    | defUse C.FP = defUseF
    | defUse _ = error "defUse"
end

(*
 * $Log: X86Props.sml,v $
 * Revision 1.2  1998/08/27 14:12:18  george
 *   used sethi-ullman number for floating point stack
 *
 * Revision 1.1.1.1  1998/07/22 18:10:32  george
 *   X86.1
 *
 *)