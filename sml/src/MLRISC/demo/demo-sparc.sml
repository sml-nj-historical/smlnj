(*---------------------------------------------------------------------------
 * Backend specific stuff.  You'll need one instance of these things 
 * for each architecture.  
 *---------------------------------------------------------------------------*)

(*
 * The Sparc instruction set, specialized with respect to the
 * user constant and region types.  
 *)
structure SparcInstr = SparcInstr
   (structure LabelExp = LabelExp
    structure Region = UserRegion
   )

(*
 * How to serialize parallel copies
 *)
structure SparcShuffle = SparcShuffle(SparcInstr)

(*
 * The assembler 
 *) 
structure SparcAsm = SparcAsmEmitter
   (structure Instr = SparcInstr
    structure Stream = Stream
    structure Shuffle = SparcShuffle
    val V9 = false  (* we'll generate V8 instructions for now *)
   )

(*
 * The flowgraph (cluster) representation specialized to the sparc instruction 
 * set.
 *)
structure SparcFlowGraph = 
   FlowGraph(structure I = SparcInstr 
             structure P = UserPseudoOps
            )
(*
 * Because of various Sparc related ugliness.  Pseudo instructions 
 * related to integer multiplication/division are handled via callbacks.  
 * Here we can decide what actual code to generate.  Here we only
 * handle a subset of of the pseudo instructions.
 *)
structure SparcPseudoInstrs =
struct
  structure I = SparcInstr
  structure C = SparcInstr.C

  type format1 =
       {r:int, i:I.operand, d:int} *
       (I.operand -> I.C.cell) -> I.instruction list

  type format2 =
       {i:I.operand, d:int} *
       (I.operand -> I.C.cell) -> I.instruction list

  fun error msg = MLRiscErrorMsg.impossible ("SparcPseudoInstrs."^msg)

  fun umul32({r, i, d}, reduceOpnd) = [I.ARITH{a=I.UMUL,r=r,i=i,d=d}]
  fun smul32({r, i, d}, reduceOpnd) = [I.ARITH{a=I.SMUL,r=r,i=i,d=d}]
  fun udiv32({r,i,d},reduceOpnd) = 
      [I.WRY{r=0,i=I.REG 0},I.ARITH{a=I.UDIV,r=r,i=i,d=d}]

  fun sdiv32({r,i,d},reduceOpnd) =
  let val t1 = C.newReg()
  in  [I.SHIFT{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
       I.WRY{r=t1,i=I.REG 0},
       I.ARITH{a=I.SDIV,r=r,i=i,d=d}
      ]
  end

  fun cvti2d({i,d},reduceOpnd) = error "cvti2d"
    (* There is no data path between integer and floating point registers.
       So we actually have to use some memory location for temporary
       This is commented out for now.
     *)
    (* 
      [I.STORE{s=I.ST,r=C.stackptrR,i=floatTmpOffset,d=reduceOpnd i,mem=stack},
       I.FLOAD{l=I.LDF,r=C.stackptrR,i=floatTmpOffset,d=d,mem=stack},
       I.FPop1{a=I.FiTOd,r=d,d=d}
      ]
    *)
  fun cvti2s _ = error "cvti2s"
  fun cvti2q _ = error "cvti2q"

  fun smul32trap _ = error "smul32trap"
  fun sdiv32trap _ = error "sdiv32trap"

  val overflowtrap32 = [] (* not needed *)
  val overflowtrap64 = [] (* not needed *)
end


(*
 * Instruction selection module for Sparc.  
 *)
structure SparcMLTreeComp = 
   Sparc(structure SparcInstr = SparcInstr
         structure SparcMLTree = MLTree
         structure PseudoInstrs = SparcPseudoInstrs
         structure ExtensionComp = UserMLTreeExtComp
           (structure I = SparcInstr
            structure T = SparcMLTree
           )
         (* Some sparc specific parameters *)
         val V9 = false
         val muluCost = ref 5
         val multCost = ref 3
         val divuCost = ref 5
         val divtCost = ref 5
         val registerwindow = ref false
         val useBR = ref false
        )


(*---------------------------------------------------------------------------
 * Okay.  Finally, we can tie the front-end and back-end together.
 *---------------------------------------------------------------------------*)
structure SparcBackEnd = 
   BackEnd
   (structure I          = SparcInstr
    structure Flowgraph  = SparcFlowGraph
    structure InsnProps  = SparcProps(SparcInstr)
    structure Rewrite    = SparcRewrite(SparcInstr)
    structure Asm        = SparcAsm
    structure MLTreeComp = SparcMLTreeComp

    val sp = I.C.stackptrR
    val spill = UserRegion.spill

    (* I'm assuming only r0 and the stack pointer is dedicated *)
    fun range(from,to,step) = if from > to then [] 
                              else from::range(from+step,to,step)
    val dedicatedRegs  = [I.C.stackptrR, I.C.GPReg 0]
    val dedicatedFRegs = []
    val availRegs      = SortedList.difference(
                            range(I.C.GPReg 0, I.C.GPReg 31, 1),
                            SortedList.uniq dedicatedRegs)
    val availFRegs     = range(I.C.FPReg 0, I.C.FPReg 31, 2)

    val initialSpillOffset = 0 (* This is probably wrong!!!!! *)
    val spillAreaSize = 4000

    fun pure(I.ANNOTATION{i,...}) = pure i
      | pure(I.LOAD _) = true
      | pure(I.FLOAD _) = true
      | pure(I.SETHI _) = true
      | pure(I.SHIFT _) = true
      | pure(I.FPop1 _) = true
      | pure(I.FPop2 _) = true
      | pure _ = false

    (* make copy *)
    fun copyR((rds as [_], rss as [_]), _) =
        I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
      | copyR((rds, rss), I.COPY{tmp, ...}) =
        I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
    fun copyF((fds as [_], fss as [_]), _) =
        I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
      | copyF((fds, fss), I.FCOPY{tmp, ...}) =
        I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}

    (* spill copy temp *)
    fun spillCopyTmp(I.COPY{dst,src,tmp,impl},offset) =
        I.COPY{dst=dst, src=src, impl=impl,
               tmp=SOME(I.Displace{base=sp, disp=offset})}
    fun spillFcopyTmp(I.FCOPY{dst,src,tmp,impl},offset) =
        I.FCOPY{dst=dst, src=src, impl=impl,
               tmp=SOME(I.Displace{base=sp, disp=offset})}

    (* spill register *)
    fun spillInstrR(d,offset) =
        [I.STORE{s=I.ST, r=sp, i=I.IMMED offset, d=d, mem=spill}]
    fun spillInstrF(d,offset) =
        [I.FSTORE{s=I.STDF, r=sp, i=I.IMMED offset, d=d, mem=spill}]

    (* reload register *)
    fun reloadInstrR(d,offset,rest) =
        I.LOAD{l=I.LD, r=sp, i=I.IMMED offset, d=d, mem=spill}::rest
    fun reloadInstrF(d,offset,rest) =
        I.FLOAD{l=I.LDDF, r=sp, i=I.IMMED offset, d=d, mem=spill}::rest
   )
