(*
 * The Alpha instruction set, specialized with respect to the
 * user constant and region types.  
 *)
structure AlphaInstr = AlphaInstr
   (structure LabelExp = LabelExp
    structure Region = UserRegion
   )

(*
 * How to serialize parallel copies
 *)
structure AlphaShuffle = AlphaShuffle(AlphaInstr)

(*
 * The assembler 
 *) 
structure AlphaAsm = AlphaAsmEmitter
   (structure Instr = AlphaInstr
    structure Stream = Stream
    structure Shuffle = AlphaShuffle
    val V9 = false  (* we'll generate V8 instructions for now *)
   )

(*
 * The flowgraph (cluster) representation specialized to the sparc instruction 
 * set.
 *)
structure AlphaFlowGraph = 
   FlowGraph(structure I = AlphaInstr 
             structure P = UserPseudoOps
            )
(*
 * Alpha has no integer division.  So they have to be handled specially.
 * The following is stolen from Fermin's C-- source code.
 *)
structure AlphaPseudoInstrs : ALPHA_PSEUDO_INSTR =
struct

  fun error msg = MLRiscErrorMsg.error ("AlphaPseudoInstrs", msg)

  structure I = AlphaInstr
  structure T = MLTree
  structure C = I.C

  type reduceOpnd = I.operand -> int

  (* reduceOpnd moves the operand to a register if it's not in one 
     already (handy).
     div*, rem* are assembler macros. The alpha/osf assembler accepts 
        divl $1, 7, $1
     but the alpha/linux assembler insists that the operand be a register
     Sigh ...
   *)

  val temps = foldr C.addReg C.empty [23, 24, 25, 26, 28]

  fun pseudoArith instr ({ra, rb, rc}, reduceOpnd) =
      [I.PSEUDOARITH{oper=instr, ra=ra, rb=I.REGop(reduceOpnd rb), rc=rc, tmps=temps}]

  fun divl  operands = pseudoArith I.DIVL operands
  fun divlu operands = pseudoArith I.DIVLU operands
  fun divq  operands = pseudoArith I.DIVQ  operands
  fun divqu operands = pseudoArith I.DIVQU operands
  fun divlv _ = error "divlv"
  fun divqv _ = error "divqv"

  fun reml  operands = pseudoArith I.REML  operands
  fun remlu operands = pseudoArith I.REMLU operands
  fun remq  operands = pseudoArith I.REMQ  operands
  fun remqu operands = pseudoArith I.REMQU operands
  fun remlv _ = error "remlv"
  fun remqv _ = error "remqv"

  val stack = I.Region.stack
  val sp = C.stackptrR

  val push16 = I.LDA{r=sp, b=sp, d=I.IMMop (~16)}
  val pop16  = I.LDA{r=sp, b=sp, d=I.IMMop 16}

  (**** int to float ****)

  (* i32 -> f32 *)
  fun cvtls({opnd, fd}, reduceOpnd) =
  let val ra = reduceOpnd opnd
  in
      [push16,
       I.STORE{stOp=I.STQ, r=ra, b=sp, d=I.IMMop 0, mem=stack},
       I.FLOAD{ldOp=I.LDT, r=fd, b=sp, d=I.IMMop 0, mem=stack},
       pop16,
       I.FUNARY{oper=I.CVTQS, fb=fd, fc=fd}]
  end

  (* i32 -> f64 *)
  fun cvtlt({opnd, fd}, reduceOpnd) =
  let val ra = reduceOpnd opnd
  in
      [push16,
       I.STORE{stOp=I.STQ, r=ra, b=sp, d=I.IMMop 0, mem=stack},
       I.FLOAD{ldOp=I.LDT, r=fd, b=sp, d=I.IMMop 0, mem=stack},
       pop16,
       I.FUNARY{oper=I.CVTQT, fb=fd, fc=fd}]
  end

  (* i64 -> f32 *)
  val  cvtqs = cvtls

  (* i64 -> f64 *)
  val cvtqt = cvtlt

  (**** float to int ****)

  (* TODO: These should really look at the rounding mode, and not generate
           CVTTQ_C blindly *)

  (* f32 -> i32 *)
  fun cvtsl({mode, fs, rd}) = let
      val ftmp = AlphaCells.newFreg()
      in
      [I.FUNARY{oper=I.CVTTQC, fb=fs, fc=ftmp},
       push16,
       I.FSTORE{stOp=I.STT, r=ftmp, b=sp, d=I.IMMop 0, mem=stack},
       I.LOAD  {ldOp=I.LDL, r=rd,   b=sp, d=I.IMMop 0, mem=stack},
       pop16
      ]
      end

  (* f64 -> i32 *)
  val cvttl= cvtsl


  (* f32 -> i64 *)
  fun cvtsq({mode, fs, rd}) = let
      val ftmp = AlphaCells.newFreg()
      in
      [I.FUNARY{oper=I.CVTTQC, fb=fs, fc=ftmp},
       push16,
       I.FSTORE{stOp=I.STT, r=ftmp, b=sp, d=I.IMMop 0, mem=stack},
       I.LOAD  {ldOp=I.LDQ, r=rd,   b=sp, d=I.IMMop 0, mem=stack},
       pop16
      ]
      end

  (* f64 -> i64 *)
  val cvttq = cvtsq


end (* AlphaPseudoInstrs *)

(*
 * Instruction selection module for Alpha.  
 *)
structure AlphaMLTreeComp = 
   Alpha(structure AlphaInstr = AlphaInstr
         structure AlphaMLTree = MLTree
         structure PseudoInstrs = AlphaPseudoInstrs
         structure ExtensionComp = UserMLTreeExtComp
           (structure I = AlphaInstr
            structure T = AlphaMLTree
           )
         (* Some alpha specific parameters *)
         val mode32bit = false (* simulate 32 bit mode *)
         val multCost = ref 8 (* just guessing *)
         val useMultByConst = ref false (* just guessing *)
         val byteWordLoadStores = ref false
         val SMLNJfloatingPoint = false (* must be true for SML/NJ *)
        )


(*
 * Alpha specific backend
 *)
structure AlphaBackEnd =
   BackEnd
   (structure I          = AlphaInstr
    structure Flowgraph  = AlphaFlowGraph
    structure InsnProps  = AlphaProps(AlphaInstr)
    structure Rewrite    = AlphaRewrite(AlphaInstr)
    structure Asm        = AlphaAsm
    structure MLTreeComp = AlphaMLTreeComp

    val sp = I.C.stackptrR
    val spill = UserRegion.spill

    (* I'm assuming only r31 and the stack pointer is dedicated *)
    fun range(from,to) = if from > to then [] 
                         else from::range(from+1,to)
    val dedicatedRegs  = [I.C.stackptrR, I.C.GPReg 31]
    val dedicatedFRegs = [I.C.FPReg 31]
    val availRegs      = SortedList.difference(
                            range(I.C.GPReg 0, I.C.GPReg 31),
                            SortedList.uniq dedicatedRegs)
    val availFRegs     = range(I.C.FPReg 0, I.C.FPReg 30)

    val initialSpillOffset = 0 (* This is probably wrong!!!!! *)
    val spillAreaSize = 4000

    fun pure _ = false

    (* make copies *)
    fun copyR((rds as [_], rss as [_]), _) =
        I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
      | copyR((rds, rss), I.COPY{tmp, ...}) =
        I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
    fun copyF((fds as [_], fss as [_]), _) =
        I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
      | copyF((fds, fss), I.FCOPY{tmp, ...}) =
        I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}

    (* spill copy temp *)
    fun spillCopyTmp(I.COPY{tmp,dst,src,impl},loc) =
        I.COPY{tmp=SOME(I.Displace{base=sp, disp=loc}),
               dst=dst,src=src,impl=impl}
    fun spillFcopyTmp(I.FCOPY{tmp,dst,src,impl},loc) =
        I.FCOPY{tmp=SOME(I.Displace{base=sp, disp=loc}),
                dst=dst,src=src,impl=impl}

    (* spill register *)
    fun spillInstrR(r,offset) =
        [I.STORE{stOp=I.STL, b=sp, d=I.IMMop offset, r=r, mem=spill}]
    fun spillInstrF(r,offset) =
        [I.FSTORE{stOp=I.STT, b=sp, d=I.IMMop offset, r=r, mem=spill}]

    (* reload register *)
    fun reloadInstrR(r,offset,rest) =
        I.LOAD{ldOp=I.LDL, b=sp, d=I.IMMop offset, r=r, mem=spill}::rest
    fun reloadInstrF(r,offset,rest) =
        I.FLOAD{ldOp=I.LDT, b=sp, d=I.IMMop offset, r=r, mem=spill}::rest
   )

