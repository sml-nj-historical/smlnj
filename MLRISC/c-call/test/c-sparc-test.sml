local 

(*---------------------------------------------------------------------------
 * First, some front-end dependent stuff.  Typically, you only need
 * one instance of these things for each source language.
 *---------------------------------------------------------------------------*)

(*
 * User defined constant type.  Dummy for now.
 * In practice, you'll want to use this type to implement constants with
 * values that cannot be determined until final code generation, e.g.
 * stack frame offset.
 *)
structure UserConst =
struct
   type const = unit
   fun toString() = ""  
   fun hash() = 0w0  
   fun valueOf _ = 0
   fun == _ = true  
end

(*
 * Instantiate label expressions with respect to user defined constants.
 * This type is somewhat misnamed; it is used to represent constant 
 * expressions.
 *)
(* structure LabelExp = LabelExp(UserConst) *)

(*
 * User defined datatype for representing aliasing.   Dummy for now.
 * You'll need this to represent aliasing information. 
 *)
structure UserRegion =
struct
   type region = unit
   fun toString () = "" 
   val memory = ()
   val stack = ()
   val readonly = ()
   val spill = ()
end

(*
 * User defined datatype for representing pseudo assembly operators.
 * Dummy for now.
 *
 * You'll need this to represent assembler directives. 
 *)
structure UserPseudoOps =
struct
   type pseudo_op = unit  
   fun toString () = ""
   fun emitValue _ = ()
   fun sizeOf _ = 0
   fun adjustLabels _ = true
end


(*
 * Instruction stream datatype.
 * This is just a simple record type used by MLRISC to represent 
 * instruction streams.
 *)
(*structure Stream = InstructionStream(UserPseudoOps)*)

(*
 * Client defined extensions.  None for now.
 * You'll need this only if you need to extend the set of MLTREE operators
 *)
structure UserExtension =
struct

   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) SparcInstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit

end

structure SparcMLTree =
   MLTreeF (structure Constant  = UserConst
    structure Region    = UserRegion
    structure Extension = UserExtension)

(*
 * This module controls how we handle user extensions.  Since we don't
 * have any yet.  This is just a bunch of dummy routines.
 *)
functor SparcMLTreeExtComp
   (structure T : MLTREE 
   		where Extension = UserExtension
    structure I : SPARCINSTR
    		where T = T
    structure Stream : MLTREE_STREAM
                where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH 
    		where I = I
                  and P = Stream.S.P
   ) : MLTREE_EXTENSION_COMP =
struct
   structure TS = Stream
   structure I = I
   structure T = I.T
   structure C = I.C
   structure Ext = UserExtension
   structure CFG = CFG
   structure SparcCompInstrExt = 
     SparcCompInstrExt(structure I = I structure CFG = CFG structure TS=Stream)

   type reducer = 
     (I.instruction,C.cellset,I.operand,I.addressing_mode, CFG.cfg) TS.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "SparcMLTreeExtComp" 

   val compileSext  = SparcCompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   val compileFext  = unimplemented
end

(*---------------------------------------------------------------------------
 * Backend specific stuff.  You'll need one instance of these things 
 * for each architecture.  
 *---------------------------------------------------------------------------*)

(*
 * The Sparc instruction set, specialized with respect to the
 * user constant and region types.  
 *)
structure SparcInstr = SparcInstr
   (SparcMLTree
   )

(*
 * How to serialize parallel copies
 *)
structure SparcShuffle = SparcShuffle(SparcInstr)

structure SparcMLTreeEval =
   MLTreeEval (structure T = SparcMLTree
    fun eq _ _ = false
    val eqRext = eq val eqFext = eq
    val eqCCext = eq val eqSext = eq)

functor SparcPseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = SparcGasPseudoOps (
    structure T = SparcMLTree
    structure MLTreeEval = SparcMLTreeEval)

structure SparcPseudoOps = SparcPseudoOpsFn(
            structure T = SparcMLTree
            structure MLTreeEval = SparcMLTreeEval)

structure PseudoOps =
  struct

    structure Client =
      struct
	structure AsmPseudoOps = SparcPseudoOps
	type pseudo_op = unit
			 
	fun toString () = ""
  
	fun emitValue _ = raise Fail "todo"
	fun sizeOf _ = raise Fail "todo"
	fun adjustLabels _ = raise Fail "todo"
      end (* Client *)
  
    structure PseudoOps = PseudoOps (structure Client = Client)
  end

structure Stream = InstructionStream(PseudoOps.PseudoOps)

(*
 * The assembler 
 *) 
structure SparcAsm = SparcAsmEmitter
   (structure Instr = SparcInstr
    structure Stream = Stream
    structure Shuffle = SparcShuffle
    structure S = Stream
    structure MLTreeEval = SparcMLTreeEval
    val V9 = false  (* we'll generate V8 instructions for now *)
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
       {r:CellsBasis.cell, i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  type format2 =
       {i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  fun error msg = MLRiscErrorMsg.impossible ("SparcPseudoInstrs."^msg)

  fun umul32({r, i, d}, reduceOpnd) = [I.ARITH{a=I.UMUL,r=r,i=i,d=d}]
  fun smul32({r, i, d}, reduceOpnd) = [I.ARITH{a=I.SMUL,r=r,i=i,d=d}]
  fun udiv32({r,i,d},reduceOpnd) = 
      [I.WRY{r=C.r0,i=I.REG(C.r0)},I.ARITH{a=I.UDIV,r=r,i=i,d=d}]

  fun sdiv32({r,i,d},reduceOpnd) =
  let val t1 = C.newReg()
  in  [I.SHIFT{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
       I.WRY{r=t1,i=I.REG(C.r0)},
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

structure SparcMLTreeHash = 
    MLTreeHash
       (structure T = SparcMLTree
        fun h _ _ = 0w0
        val hashRext = h	val hashFext = h
        val hashCCext = h       val hashSext = h)

structure SparcProps = 
  SparcProps
    (structure SparcInstr = SparcInstr
     structure MLTreeEval = SparcMLTreeEval
     structure MLTreeHash = SparcMLTreeHash)

structure SparcAsmEmitter = 
  SparcAsmEmitter(structure Instr=SparcInstr
		  structure Shuffle=SparcShuffle
                  structure S = Stream
		  structure MLTreeEval=SparcMLTreeEval
                  val V9 = false)


structure SparcCFG = 
  ControlFlowGraph
     (structure I = SparcInstr
      structure PseudoOps = SparcPseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = SparcProps
      structure Asm = SparcAsmEmitter)

(*
    structure MLTreeComp=
       Sparc(structure SparcInstr = SparcInstr
             structure SparcMLTree = SparcMLTree
             structure PseudoInstrs = SparcPseudoInstrs
             structure ExtensionComp = SparcMLTreeExtComp
               (structure I = SparcInstr
                structure T = SparcMLTree
                structure Stream = Stream
		structure CFG = SparcCFG
               )
             val V9 = false
             val muluCost = ref 5
             val multCost = ref 3
             val divuCost = ref 5
             val divtCost = ref 5
             val registerwindow = ref false
             val useBR = ref false
            )
*)
(*
(*---------------------------------------------------------------------------
 * Okay.  Finally, we can tie the front-end and back-end together.
 *---------------------------------------------------------------------------*)
structure SparcBackEnd = 
   BackEnd
   (structure Flowgraph  = SparcFlowGraph
    structure MLTreeComp = SparcMLTreeComp
    structure Asm        = SparcAsm

    structure RA =
      RISC_RA 
      (structure I         = SparcInstr
       structure Flowgraph = Flowgraph
       structure Asm       = Asm
       structure InsnProps = InsnProps
       structure Spill     = RASpill(structure Asm = Asm
                                     structure InsnProps = InsnProps)
       structure Rewrite   = SparcRewrite(SparcInstr)
       structure SpillHeur = ChaitinSpillHeur
       structure C         = I.C
 
       val sp = C.stackptrR
       val spill = UserRegion.spill 

       structure SpillTable = SpillTable
           (val initialSpillOffset = 0 (* This is probably wrong!!!!! *)
            val spillAreaSz = 4000
            val architecture = "Sparc" 
           )
       open SpillTable
   
       fun pure(I.ANNOTATION{i,...}) = pure i
         | pure(I.LOAD _) = true
         | pure(I.FLOAD _) = true
         | pure(I.SETHI _) = true
         | pure(I.SHIFT _) = true
         | pure(I.FPop1 _) = true
         | pure(I.FPop2 _) = true
         | pure _ = false
   
       (* I'm assuming only r0 and the stack pointer is dedicated *)
       structure Int =
       struct
           val dedicated  = [I.C.stackptrR, I.C.GPReg 0]
           val avail = 
             C.SortedCells.return
              (C.SortedCells.difference(
                C.SortedCells.uniq(
                  C.Regs C.GP {from=0, to=31, step=1}),
                C.SortedCells.uniq dedicated)
              )

          fun copy((rds as [_], rss as [_]), _) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
            | copy((rds, rss), I.COPY{tmp, ...}) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}

          (* spill copy temp *)
          fun spillCopyTmp(_,I.COPY{dst,src,tmp,impl},loc) =
              I.COPY{dst=dst, src=src, impl=impl,
                     tmp=SOME(I.Displace{base=sp, disp=get loc})}
       
          (* spill register *)
           fun spillInstr{an,src,spilledCell,spillLoc} =
               [I.STORE{s=I.ST, r=sp, i=I.IMMED(get spillLoc), d=src, 
                      mem=spill}]
           
          (* reload register *)
           fun reloadInstr{an,dst,spilledCell,spillLoc} =
                [I.LOAD{l=I.LD, r=sp, i=I.IMMED(get spillLoc), d=dst, 
                      mem=spill}]
       end

       structure Float = 
       struct
          val dedicated = []
          val avail     = C.Regs C.FP {from=0, to=31, step=2}
   
          fun copy((fds as [_], fss as [_]), _) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
            | copy((fds, fss), I.FCOPY{tmp, ...}) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}
   
          fun spillCopyTmp(_,I.FCOPY{dst,src,tmp,impl},loc) =
              I.FCOPY{dst=dst, src=src, impl=impl,
                      tmp=SOME(I.Displace{base=sp, disp=getF loc})}
   
          fun spillInstr(_, d,loc) =
              [I.FSTORE{s=I.STDF, r=sp, i=I.IMMED(getF loc), d=d, mem=spill}]
   
          fun reloadInstr(_,d,loc) =
              [I.FLOAD{l=I.LDDF, r=sp, i=I.IMMED(getF loc), d=d, mem=spill}]
       end
      )
   )
*)
in
structure SparcTest = struct end
end
