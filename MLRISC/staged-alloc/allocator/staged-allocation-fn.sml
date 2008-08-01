c-call/cm/group.cm                                                                                  0000644 0000765 0000765 00000002043 11044655151 014172  0                                                                                                    ustar   mrainey                         mrainey                                                                                                                                                                                                                Group

	signature C_CALL

	functor X86SVIDFn
	functor X86_64SVIDFn
	functor SparcCCallFn

	structure X86MLRISCGen
	structure X86MLTree
        structure X86Test
	structure X86_64Test
	structure SparcTest
	structure CType

	group(../../staged-alloc/cm/sources.cm)
	library(../../cm/MLRISC.cm)
	library(../../cm/IA32.cm)
	library(../../cm/AMD64.cm)
is

	$/basis.cm
	$/smlnj-lib.cm
	$/controls-lib.cm
	$/pp-lib.cm
	$smlnj-tdp/plugins.cm

	../../cm/Control.cm
	../../cm/Lib.cm
	../../cm/MLRISC.cm
	../../cm/Graphs.cm
	../../cm/MLTREE.cm
	../../cm/RA.cm
	../../cm/Visual.cm
	../../cm/Peephole.cm
	../../cm/IA32.cm
	../../cm/AMD64.cm
	../../cm/RTL.cm
	../../cm/SPARC.cm

	../gen/c-call-sig.sml
	../gen/c-call-fn.sml
	../gen/c-type.sml

	../../staged-alloc/cm/sources.cm

	../archs/x86-64-svid-fn.sml
	../archs/x86-64-c-sizes.sml
	../archs/x86-svid-fn.sml
	../archs/x86-c-sizes.sml
	../archs/sparc-c-sizes.sml
	../archs/sparc-c-call-fn.sml

	../test/c-test-gen.sml
	../test/c-x86-test.sml
	../test/c-x86-64-test.sml
	../test/c-sparc-test.sml
	../test/spill-table.sml                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             c-call/test/c-sparc-test.sml                                                                        0000644 0000765 0000765 00000037777 11044707570 016150  0                                                                                                    ustar   mrainey                         mrainey                                                                                                                                                                                                                local 

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

structure SparcStream = InstructionStream(PseudoOps.PseudoOps)
structure SparcMLTreeStream = 
    MLTreeStream
      (structure T = SparcMLTree
       structure S = SparcStream)

(*
 * The assembler 
 *) 
structure SparcAsm = SparcAsmEmitter
   (structure Instr = SparcInstr
    structure Stream = SparcStream
    structure Shuffle = SparcShuffle
    structure S = SparcStream
    structure MLTreeEval = SparcMLTreeEval
    val V9 = false  (* we'll generate V8 instructions for now *)
   )

structure SparcPseudoInstrs : SPARC_PSEUDO_INSTR = 
struct
  structure I = SparcInstr
  structure C = I.C

  type format1 =
       {r:CellsBasis.cell, i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  type format2 =
       {i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  fun error msg = MLRiscErrorMsg.impossible ("SparcPseudoInstrs."^msg)

  val delta = 0 (*SparcSpec.framesize*)	(* initial value of %fp - %sp *)

  (* runtime system dependent; the numbers are relative to %sp but
   * we need offsets relative to %fp, hence the adjustment by delta *)
  val floatTmpOffset = I.IMMED (88 - delta)
  val umulOffset = I.IMMED (80 - delta)
  val smulOffset = I.IMMED (72 - delta)
  val udivOffset = I.IMMED (84 - delta)
  val sdivOffset = I.IMMED (76 - delta)

  val stack = () (*CPSRegions.stack*)

  val native = true  (* use native versions of the instructions? *)

  fun umul_native({r, i, d}, reduceOpnd) =
      [I.arith{a=I.UMUL,r=r,i=i,d=d}]

  val TNE = I.ticc{t=I.BNE,cc=I.ICC,r=C.r0,i=I.IMMED 7}
  val TVS = I.ticc{t=I.BVS,cc=I.ICC,r=C.r0,i=I.IMMED 7}

      (* overflows iff Y != (d ~>> 31) *)
  fun smult_native({r, i, d}, reduceOpnd) =
      let val t1 = C.newReg()
          val t2 = C.newReg()
      in  [I.arith{a=I.SMUL,r=r,i=i,d=d},
           I.shift{s=I.SRA,r=d,i=I.IMMED 31,d=t1},
           I.rdy{d=t2},
           I.arith{a=I.SUBCC,r=t1,i=I.REG t2,d=C.r0},
           TNE
          ] 
      end

  fun smul_native({r, i, d}, reduceOpnd) =
      [I.arith{a=I.SMUL,r=r,i=i,d=d}]

  fun udiv_native({r,i,d},reduceOpnd) = 
      [I.wry{r=C.r0,i=I.REG C.r0},
       I.arith{a=I.UDIV,r=r,i=i,d=d}]

   (* May overflow if MININT div -1 *)
  fun sdivt_native({r,i,d},reduceOpnd) = 
      let val t1 = C.newReg()
      in  [I.shift{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.wry{r=t1,i=I.REG C.r0},
           I.arith{a=I.SDIVCC,r=r,i=i,d=d},
           TVS
          ]
      end

  fun sdiv_native({r,i,d},reduceOpnd) =
      let val t1 = C.newReg()
      in  [I.shift{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.wry{r=t1,i=I.REG C.r0},
           I.arith{a=I.SDIV,r=r,i=i,d=d}
          ]
      end

  (* 
   * Registers %o2, %o3 are used to pass arguments to ml_mul and ml_div 
   * Result is returned in %o2.
   *)
  val r10 = C.GPReg 10
  val r11 = C.GPReg 11

  fun callRoutine(offset,reduceOpnd,r,i,d) =   
  let val addr = C.newReg()
      val defs = C.addReg(r10,C.empty) 
      val uses = C.addReg(r10,C.addReg(r11,C.empty))
      fun copy{dst, src, tmp} = 
	  I.COPY{k=CellsBasis.GP, sz=32, dst=dst, src=src, tmp=tmp}
  in
      [copy{src=[r,reduceOpnd i],dst=[r10,r11],tmp=SOME(I.Direct(C.newReg()))},
       I.load{l=I.LD,r=C.frameptrR,i=offset,d=addr,mem=stack},
       I.jmpl{r=addr,i=I.IMMED 0,d=C.linkReg,defs=defs,uses=uses,
              cutsTo=[],nop=true,mem=stack},
       copy{src=[r10],dst=[d],tmp=NONE}
      ]
  end

  fun umul({r, i, d}, reduceOpnd) = callRoutine(umulOffset,reduceOpnd,r,i,d)
  fun smultrap({r, i, d}, reduceOpnd) = callRoutine(smulOffset,reduceOpnd,r,i,d)
  fun udiv({r, i, d}, reduceOpnd) = callRoutine(udivOffset,reduceOpnd,r,i,d)
  fun sdivtrap({r, i, d}, reduceOpnd) = callRoutine(sdivOffset,reduceOpnd,r,i,d)

  fun cvti2d({i, d}, reduceOpnd) = 
      [I.store{s=I.ST,r=C.frameptrR,i=floatTmpOffset,d=reduceOpnd i,mem=stack},
       I.fload{l=I.LDF,r=C.frameptrR,i=floatTmpOffset,d=d,mem=stack},
       I.fpop1{a=I.FiTOd,r=d,d=d}
      ]
  fun cvti2s _ = error "cvti2s"
  fun cvti2q _ = error "cvti2q"

     (* Generate native versions of the instructions *)
  val umul32 = if native then umul_native else umul
  val smul32 : format1 =
      if native then smul_native else (fn _ => error "smul32")
  val smul32trap = if native then smult_native else smultrap
  val udiv32 = if native then udiv_native else udiv
  val sdiv32 : format1 =
      if native then sdiv_native else (fn _ => error "sdiv32")
  val sdiv32trap = if native then sdivt_native else sdivtrap

  val overflowtrap32 = (* tvs 0x7 *)
                       [I.ticc{t=I.BVS,cc=I.ICC,r=C.r0,i=I.IMMED 7}]
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
                  structure S = SparcStream
		  structure MLTreeEval=SparcMLTreeEval
                  val V9 = false)


structure SparcCFG = 
  ControlFlowGraph
     (structure I = SparcInstr
      structure PseudoOps = SparcPseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = SparcProps
      structure Asm = SparcAsmEmitter)

structure SparcFlowGraph = BuildFlowgraph 
	    (structure Props = SparcProps
             structure Stream = SparcStream
	     structure CFG = SparcCFG)

structure SparcExpand = CFGExpandCopies (structure CFG=SparcCFG
                                         structure Shuffle = SparcShuffle)
structure SparcBlockPlacement = DefaultBlockPlacement(SparcCFG)

structure SparcEmit = CFGEmit (
             structure CFG = SparcCFG
             structure E = SparcAsmEmitter) 

structure SparcCCall = SparcCCallFn (
		         structure T = SparcMLTree
			 fun ix x = raise Fail "")

(*
 * This module controls how we handle user extensions.  Since we don't
 * have any yet.  This is just a bunch of dummy routines.
 *)
structure SparcMLTreeExtComp : MLTREE_EXTENSION_COMP =
struct
   structure TS = SparcMLTreeStream
   structure I = SparcInstr
   structure T = SparcMLTree
   structure C = I.C
   structure Ext = UserExtension
   structure CFG = SparcCFG
   structure SparcCompInstrExt = 
     SparcCompInstrExt(structure I = I structure CFG = CFG structure TS=SparcMLTreeStream)

   type reducer = 
     (I.instruction,C.cellset,I.operand,I.addressing_mode, CFG.cfg) TS.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "SparcMLTreeExtComp" 

   val compileSext  = SparcCompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   val compileFext  = unimplemented
end

    structure MLTreeComp=
       Sparc(structure SparcInstr = SparcInstr
             structure SparcMLTree = SparcMLTree
             structure PseudoInstrs = SparcPseudoInstrs
             structure ExtensionComp = SparcMLTreeExtComp
             val V9 = false
             val muluCost = ref 5
             val multCost = ref 3
             val divuCost = ref 5
             val divtCost = ref 5
             val registerwindow = ref false
             val useBR = ref false
            )


    structure InsnProps = SparcProps

    structure RA = 
       RISC_RA
         (structure I         = SparcInstr
       structure C         = CellsBasis
       structure T = SparcMLTree
          structure CFG       = SparcCFG
          structure InsnProps = InsnProps 
          structure Rewrite   = SparcRewrite(SparcInstr)
	  structure SpillInstr= SparcSpillInstr(SparcInstr)
          structure Asm       = SparcAsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = SparcAsmEmitter)

          structure SpillTable = SpillTable(val initialSpillOffset = 0 (* This is probably wrong!!!!! *)
            val spillAreaSz = 4000
            val architecture = "Sparc" )
          val fp = I.C.frameptrR
          val spill = UserRegion.spill
	  datatype spillOperandKind = SPILL_LOC | CONST_VAL
	  type spill_info = unit
          fun beforeRA _ = SpillTable.beginRA()

          val architecture = "Sparc"
         
          fun pure(I.ANNOTATION{i,...}) = pure i
            | pure(I.INSTR(I.LOAD _)) = true
            | pure(I.INSTR(I.FLOAD _)) = true
            | pure(I.INSTR(I.SETHI _)) = true
            | pure(I.INSTR(I.SHIFT _)) = true
            | pure(I.INSTR(I.FPop1 _)) = true
            | pure(I.INSTR(I.FPop2 _)) = true
            | pure _ = false

          (* make copy *) 
          structure Int = 
          struct
	               val dedicated = [I.C.stackptrR, I.C.GPReg 0]
             val avail     = 
		 C.SortedCells.return
              (C.SortedCells.difference(
                C.SortedCells.uniq(
                   SparcCells.Regs C.GP {from=0, to=31, step=1}),
                C.SortedCells.uniq dedicated)
              )

	     fun mkDisp loc = T.LI(T.I.fromInt(32, SpillTable.get loc))
             fun spillLoc{info, an, cell, id} = 
		  {opnd=I.Displace{base=fp, disp=mkDisp(RAGraph.FRAME id), mem=spill},
		   kind=SPILL_LOC}

             val mode = RACore.NO_OPTIMIZATION
          end

          structure Float = 
          struct
      fun fromto(n, m, inc) = if n>m then [] else n :: fromto(n+inc, m, inc)
	  val avail =  SparcCells.Regs C.FP {from=0, to=30, step=2}
	  val dedicated = []

	      fun mkDisp loc = T.LI(T.I.fromInt(32, SpillTable.getF loc))

             fun spillLoc(S, an, loc) = 
		I.Displace{base=fp, disp=mkDisp(RAGraph.FRAME loc), mem=spill}

             val mode = RACore.NO_OPTIMIZATION
          end
         )

structure Cells = SparcInstr.C
structure T = SparcMLTree
structure CFG = SparcCFG
structure FlowGraph = SparcFlowGraph
    val wordTy = 32

    fun gen (functionName, stms, result) = let
           val insnStrm = FlowGraph.build()
	   val stream as SparcStream.STREAM
           { beginCluster,  (* start a cluster *)
             endCluster,    (* end a cluster *)
             emit,          (* emit MLTREE stm *)
             defineLabel,   (* define a local label *)
             entryLabel,    (* define an external entry *)
             exitBlock,     (* mark the end of a procedure *)
             pseudoOp,      (* emit a pseudo op *)
             annotation,    (* add an annotation *)
             ... } =
             MLTreeComp.selectInstructions insnStrm
	fun doit () = (
	    beginCluster 0;      (* start a new cluster *)
            pseudoOp PseudoOpsBasisTyp.TEXT;		  
	    pseudoOp (PseudoOpsBasisTyp.EXPORT [functionName]);    
            entryLabel functionName; (* define the entry label *)
            List.app emit stms; (* emit all the statements *)
            exitBlock result;
            endCluster [])
	val cfg = doit ()
	val cfg = RA.run cfg
	val cfg = SparcExpand.run cfg
        in  
         (cfg, stream)        (* end the cluster *)
       end

    fun dumpOutput (cfg, stream) = let
	val (cfg as Graph.GRAPH graph, blocks) = 
		SparcBlockPlacement.blockPlacement cfg
	val CFG.INFO{annotations=an, data, decls, ...} = #graph_info graph
	in
	  SparcEmit.asmEmit (cfg, blocks)
	end (* dumpOutput *)

   
    fun codegen (functionName, target, proto, initStms, args) = let 
        val _ = Label.reset()

	val [functionName, target] = List.map Label.global [functionName, target]

	(* construct the C call *)
	val {result, callseq} = SparcCCall.genCall {
	           name=T.LABEL target,
	           paramAlloc=fn _ => false,
(* FIXME *)
	           structRet=fn _ => T.REG(32, SparcCells.GPReg 0),
	           saveRestoreDedicated=fn _ => {save=[], restore=[]},
	           callComment=NONE,
	           proto=proto,
	           args=args}

	fun wordLit i = T.LI (T.I.fromInt (wordTy, i))

	val stms = List.concat [
		   initStms,
		   callseq, 
		   [T.RET []]]

(*	val _ = List.all (fn stm => ChkTy.check stm 
				    orelse raise Fail ("typechecking error: "^SparcMTC.SparcMLTreeUtils.stmToString stm))
		stms
*)

        in
	   dumpOutput(gen (functionName, stms, result))
	end

    val GP = SparcCells.GPReg
    val FP = SparcCells.FPReg

    fun greg r = GP r
    fun oreg r = GP (r + 8)
    fun ireg r = GP (r + 24)
    fun freg r = FP r
    fun reg32 r = T.REG (32, r)
    fun freg64 r = T.FREG (64, r)
    fun LI i = T.LI (T.I.fromInt (32, i))


in
structure SparcTest = GenTestFn (
		  structure T = SparcMLTree
		  structure CCall = SparcCCall
		  structure Cells = SparcCells
		  val codegen = codegen
		  val param0 = reg32(oreg 0)
		  val wordTy = 32)
end
 c-call/test/spill-table.sml                                                                         0000644 0000765 0000765 00000003656 11044655165 016042  0                                                                                                    ustar   mrainey                         mrainey                                                                                                                                                                                                                functor SpillTable
   (val architecture : string  
    val initialSpillOffset : int
    val spillAreaSz : int
   ) : 
sig
  
   val architecture : string
   val beginRA : unit -> unit
   val get     : RAGraph.spillLoc -> int
   val getF    : RAGraph.spillLoc -> int

end =
struct

   structure G = RAGraph

   fun error msg = MLRiscErrorMsg.error(architecture^".SpillTable",msg)
  
   val itow = Word.fromInt

   val architecture = architecture

   exception RegSpills and FregSpills
   val spillOffset = ref initialSpillOffset
   val regspills : int G.SpillLocHashTable.hash_table =
       G.SpillLocHashTable.mkTable(0,RegSpills)
   val fregspills : int G.SpillLocHashTable.hash_table =
       G.SpillLocHashTable.mkTable(0,FregSpills)
   val lookupReg  = G.SpillLocHashTable.lookup regspills
   val enterReg   = G.SpillLocHashTable.insert regspills
   val lookupFreg = G.SpillLocHashTable.lookup fregspills
   val enterFreg  = G.SpillLocHashTable.insert fregspills

   fun beginRA() =
      ((* Reset the regspills/fregspills map by need. *)
       if !spillOffset = initialSpillOffset then ()
       else (G.SpillLocHashTable.clear regspills;
             G.SpillLocHashTable.clear fregspills
            )
       ;
       spillOffset := initialSpillOffset
      )

   fun newOffset offset =
       if offset >= spillAreaSz then error "spill area too small"
       else spillOffset := offset

   (* Get spill location for integer registers *)
   fun get loc =
       lookupReg loc handle _ =>
       let val offset = !spillOffset
       in  newOffset(offset+4);
           enterReg (loc,offset);
           offset
       end

   (* Get spill location for floating point registers *)
   fun getF loc =
       lookupFreg loc handle _ =>
       let val offset = !spillOffset
           val aligned = Word.toIntX (Word.andb(itow (offset+7), itow ~8))
       in
           newOffset(aligned+8);
           enterFreg (loc, aligned);
           aligned
       end

end
                                                                                  c-call/test/Makefile                                                                                0000644 0000765 0000765 00000002547 11044707471 014551  0                                                                                                    ustar   mrainey                         mrainey                                                                                                                                                                                                                ML_BUILD_FLAGS =	-Ctdp.instrument=true \$$smlnj-tdp/back-trace.cm

build-sparc:
	ml-build $(ML_BUILD_FLAGS) ../cm/wrapper.cm SparcTest.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc -g -c mlrisc.s
	gcc -g  -c glue.c
	gcc -g  -c sanity.c
	gcc -g  mlrisc.o glue.o sanity.o -o sanity
	gcc -g  -c main.c
	gcc -g  mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

build-x86-64:
	ml-build $(ML_BUILD_FLAGS) ../cm/wrapper.cm X86_64Test.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc -g -m64 -c mlrisc.s
	gcc -g  -m64 -c glue.c
	gcc -g  -m64 -c sanity.c
	gcc -g  -m64 mlrisc.o glue.o sanity.o -o sanity
	gcc -g  -m64 -c main.c
	gcc -g  -m64 mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

X86_FLAGS=-m32 -march=i686 -g

build-x86:
	ml-build $(ML_BUILD_FLAGS) ../cm/wrapper.cm X86Test.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc $(X86_FLAGS)  -c mlrisc.s
	gcc $(X86_FLAGS)   -c glue.c
	gcc $(X86_FLAGS)   -c sanity.c
	gcc $(X86_FLAGS) -S glue.c
	gcc $(X86_FLAGS)   mlrisc.o glue.o sanity.o -o sanity
	gcc $(X86_FLAGS)   -c main.c
	gcc $(X86_FLAGS)   mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

clean:
	rm -rf *.s *.c *.o main sanity *~ .cm *.out test-main*                                                                                                                                                         c-call/varargs/interp/gen-fn.sml                                                                    0000644 0000765 0000765 00000026625 11044706720 016766  0                                                                                                    ustar   mrainey                         mrainey                                                                                                                                                                                                                (* gen-fn.sml
 *
 * Generate the interpreter loop.
 *) 

functor GenFn (
    structure T : MLTREE
  (* general-purpose registers used for passing or returning arguments *)
    val gprs : T.reg list
  (* floating-point registers used for passing or returning arguments *)
    val fprs : T.reg list
  (* possible widths *)
    val gprWidths : T.ty list
    val fprWidths : T.ty list
  (* stack pointer register *)
    val spReg : T.rexp
    val defaultWidth : T.ty
    val callerSaves : T.reg list
    val callerSavesF : T.reg list
  ) :> sig

  (* generate the machine-independent part of the vararg interpreter *)
    val gen : {interpFunPtr : T.rexp, largsReg : T.reg, endOfLargs : T.rexp} -> T.stm list

  end = struct

    structure Consts = VarargConstants
    structure SA = StagedAllocation(
		     type reg_id = T.reg
		     datatype loc_kind = datatype CLocKind.loc_kind
		     val memSize = 4)

    datatype loc 
      = REG_LOC of T.reg
      | STK_LOC

    datatype loc_kind = datatype CLocKind.loc_kind

  (* as we go from top to bottom, we become increasingly specific about the destination of the argument. *)
    datatype branch
      = ENTRY of {larg : T.rexp, ks : loc_kind list, widths : T.ty list, narrowings : T.ty list, locs : loc list}
      | KIND of {larg : T.rexp, k : loc_kind, widths : T.ty list, narrowings : T.ty list, locs : loc list}
      | WIDTH of {larg : T.rexp, k : loc_kind, width : T.ty, narrowings : T.ty list, locs : loc list}
      | NARROWING of {larg : T.rexp, k : loc_kind, width : T.ty, narrowing : T.ty, locs : loc list}
      | LOC of {larg : T.rexp, k : loc_kind, width : T.ty, narrowing : T.ty, loc : loc}

    val regToInt = CellsBasis.physicalRegisterNum
    fun locToInt (REG_LOC r) = regToInt r
      | locToInt STK_LOC = 0

  (* labels *)
    local
	val instLabels = ref ([] : (string * Label.label) list)
	fun newLabel s = (case List.find (fn (s', _) => s' = s) (!instLabels)
                of NONE => let
	           val l = Label.label s ()
		   in
		       instLabels := (s, l) :: !instLabels;
		       l
		   end
		 | SOME (s, l) => l
               (* end case *))
	fun kindToString GPR = "GPR"
	  | kindToString FPR = "FPR"
	  | kindToString STK = "STK"
	  | kindToString FSTK = "FSTK"
	val c = String.concatWith "."
	val i2s = Int.toString
	fun locToString (REG_LOC r) = "r"^i2s (regToInt r)
	  | locToString STK_LOC = "stk"
	fun instToString (ENTRY {...}) = "entry"
	  | instToString (KIND {k, ...}) = c["kind", kindToString k]
	  | instToString (WIDTH {k, width, ...}) = c["width", kindToString k, i2s width]
	  | instToString (NARROWING {k, width, narrowing, ...}) = 
	        c["narrowing", kindToString k, i2s width, i2s narrowing]
	  | instToString (LOC {k, width, narrowing, loc, ...}) = 
	        c["loc", kindToString k, i2s width, i2s narrowing, locToString loc]
    in
  (* generates labels for instructions *)
    val labelOfInst = newLabel o instToString
    val interpEntryLab = newLabel "interpEntry"
    val interpLab = newLabel "interp"
    val gotoCLab = newLabel "gotoC"
    val errLab = Label.global "vararg_error"
    end (* local *)

    val defTy = defaultWidth
    val mem = T.Region.memory
    val stack = T.Region.stack
    fun lit i = T.LI (T.I.fromInt (defTy, i))
    val lit' = lit o Word32.toInt
    fun gpr r = T.GPR (T.REG (defTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))
    fun concatMap f xs = List.concat (List.map f xs)

  (* displacement from the located argument *)
    fun offLocdArg (ty, larg, off) = T.LOAD(ty, T.ADD(defTy, larg, lit' off), mem)
    fun offLocdArgF (ty, larg, off) = T.FLOAD(ty, T.ADD(defTy, larg, lit' off), mem)

  (* store an integer argument on the stack *)
    fun storeSTK larg ty = 
	    T.STORE(ty, T.ADD (defTy, spReg, offLocdArg(defTy, larg, Consts.locOffB)), 
		    offLocdArg(ty, larg, Consts.argOffB), mem)

  (* store a floating-point argument on the stack *)
    fun storeFSTK larg ty = 
	    T.FSTORE(ty, T.ADD (defTy, spReg, offLocdArg(defTy, larg, Consts.locOffB)), 
		    offLocdArgF(ty, larg, Consts.argOffB), mem)

  (* load an integer argument into a register *)
    fun loadGPR larg ty r = T.MV(ty, r, offLocdArg(ty, larg, Consts.argOffB))

  (* load a floating-point argument into a register *)
    fun loadFPR larg ty r = T.FMV(ty, r, offLocdArgF(ty, larg, Consts.argOffB))

  (* are the width and narrowing legal for kind of location? *)
    fun widthOK (k, w, narrowing) = let
	    val ws = (case k
		       of (GPR | STK) => gprWidths
			| (FPR | FSTK) => fprWidths)
            in
	       List.exists (fn w' => w = w') ws andalso List.exists (fn w' => narrowing = w') ws
	    end

  (* generate code that places the argument *)
    fun loc {larg, k, width, narrowing, loc} = let
          (* offset into the argument (only nonzero if the argument has an aggregate type) *)
	    val argMembOff = offLocdArg(ty, larg, Consts.offsetOffB)
          (* narrow the location if necessary *)
	    fun narrow loc = if width = narrowing then loc
			     else SA.NARROW(loc, k, loc)
(* FIXME: handle narrowing and offsets *)
	    val ldInstrs = (
		case (k, loc, widthOK(k, width, narrowing))
		 of (GPR, REG_LOC r, true) => 
		    CCall.writeLoc (offLocdArg(ty, larg, Consts.argOffB)) (argMembOff, narrow(SA.REG(ty, GPR, r)), [])
(* [loadGPR larg width r] *)
		  | (FPR, REG_LOC r, true) =>
		    CCall.writeLoc (offLocdArgF(ty, larg, Consts.argOffB)) (argMembOff, narrow(SA.REG(ty, FPR, r)), [])
(*  		    [loadFPR larg width r]*)
		  | (STK, STK_LOC, true) =>
		    [storeSTK larg width]
		  | (FSTK, STK_LOC, true) =>
		    [storeFSTK larg width]
		  | _ => [T.JMP (T.LABEL errLab, [])]
               (* end case *))
            in
		 (* place the argument *)
		   ldInstrs @
		 (* return to the interpreter loop *)
		   [T.JMP (T.LABEL interpLab, [])]
	    end

    fun genHandlers (i, f, instrs) = let
	    fun genHandler instr = let
		    val lab = labelOfInst (i instr)
		    in
                      List.concat [
	               [T.DEFINE lab],
		       f instr,
		       [T.JMP (T.LABEL errLab, [])]
		    ]    
	           end
            in
	       concatMap genHandler instrs
	    end

  (* generate code to handle an argument narrowing *)
    fun narrowing {larg, k, width, narrowing, locs} = let
	  (* we only use this instruction for generating labels *)
	    fun branch loc = LOC {larg=larg, k=k, width=width, narrowing=narrowing, loc=loc}
	    val locBranches = List.map (labelOfInst o branch) locs
	    fun instr (loc, branch) = if (k = GPR orelse k = FPR)
                    then T.BCC(T.CMP(defTy, T.EQ, 
				     offLocdArg(defTy, larg, Consts.locOffB),
				     lit (locToInt loc)),
			       branch)
                    else T.JMP (T.LABEL branch, [])
            in
	       ListPair.map instr (locs, locBranches)
	    end

  (* generate code to handle an argument width *)
    fun width {larg, k, width, narrowings, locs} = let
	  (* we only use this instruction for generating labels *)
	    fun branch narrowing = NARROWING {larg=larg, k=k, width=width, narrowing=narrowing, locs=locs}
	    val narrowingBranches = List.map (labelOfInst o branch) narrowings
	    fun instr (narrowing, branch) =
		    T.BCC(T.CMP(defTy, T.EQ, 
				offLocdArg(defTy, larg, Consts.narrowingOffB),
				lit narrowing),
			  branch)
            in
	       ListPair.map instr (narrowings, narrowingBranches)
	    end

  (* generate code to handle an argument kind *)
    fun kind {larg, k, widths, narrowings, locs} = let
	    fun branch width = WIDTH {larg=larg, k=k, width=width, narrowings=narrowings, locs=locs}
	    val widthBranches = List.map (labelOfInst o branch) widths
	    fun instr (width, branch) =
		    T.BCC(T.CMP(defTy, T.EQ, 
				offLocdArg(defTy, larg, Consts.widthOffB),
				lit width),
			  branch)
            in
	       ListPair.map instr (widths, widthBranches)
	    end

  (* generate code to handle an argument kind *)
    fun entry {larg, ks, widths, narrowings, locs} = let
	    fun branch k = KIND {larg=larg, k=k, widths=widths, narrowings=narrowings, locs=locs}
	    val kBranches = List.map (labelOfInst o branch) ks
	    fun instr (k, branch) =
		    T.BCC(T.CMP(defTy, T.EQ, 
				offLocdArg(defTy, larg, Consts.kindOffB),
				lit'(Consts.kind k)),
			  branch)
            in
	       ListPair.map instr (ks, kBranches)
	    end

    fun locInstrs {larg, k, width, narrowing, locs=[]} = []
      | locInstrs {larg, k, width, narrowing, locs=loc::locs} =
	    {larg=larg, k=k, width=width, narrowing=narrowing, loc=loc} :: 
	      locInstrs {larg=larg, k=k, width=width, narrowing=narrowing, locs=locs}

    fun narrowingInstrs {larg, k, width, narrowings=[], locs} = []
      | narrowingInstrs {larg, k, width, narrowings=narrowing::narrowings, locs} =
	    {larg=larg, k=k, width=width, narrowing=narrowing, locs=locs} :: 
	      narrowingInstrs {larg=larg, k=k, width=width, narrowings=narrowings, locs=locs}

    fun widthInstrs {larg, k, widths=[], narrowings, locs} = []
      | widthInstrs {larg, k, widths=width::widths, narrowings, locs} = 
	    {larg=larg, k=k, width=width, narrowings=narrowings, locs=locs} :: 
	      widthInstrs {larg=larg, k=k, widths=widths, narrowings=narrowings, locs=locs}

    fun kindInstrs {larg, ks=[], widths, narrowings, locs} = []
      | kindInstrs {larg, ks=k::ks, widths, narrowings, locs} = 
	    {larg=larg, k=k, widths=widths, narrowings=narrowings, locs=locs} :: 
	      kindInstrs {larg=larg, ks=ks, widths=widths, narrowings=narrowings, locs=locs}

    structure IS = IntBinarySet
    fun mkUnique ints = IS.listItems(IS.addList(IS.empty, ints))

    fun entryInstr larg = let
	    val ks = [GPR, FPR, STK, FSTK]
	    val widths = mkUnique (gprWidths@fprWidths)
	    val narrowings = widths
	    val locs = STK_LOC :: List.map REG_LOC gprs @ List.map REG_LOC fprs
            in
	       {larg=larg, ks=ks, widths=widths, narrowings=narrowings, locs=locs}
	    end

  (* all possible combinations of instructions *)
    fun allInstrs larg = let
	    val entryInstr = entryInstr larg
	    val kindInstrs = kindInstrs entryInstr
	    val widthInstrs = concatMap widthInstrs kindInstrs
	    val narrowingInstrs = concatMap narrowingInstrs widthInstrs
	    val locInstrs = concatMap locInstrs narrowingInstrs 
            in
	         (entryInstr, kindInstrs, widthInstrs, narrowingInstrs, locInstrs)
	    end

  (* call the varargs C function *)
    fun genCallC interpFunPtr = let
	   val defs = List.map gpr callerSaves @ List.map (fn r => fpr(64, r)) callerSavesF
	   val uses = List.map gpr gprs @ List.map (fn r => fpr(64, r)) fprs
	   in
	      [
	       T.DEFINE gotoCLab,
	       T.CALL {funct=interpFunPtr, targets=[], defs=defs, uses=uses, region=mem, pops=0}
	      ]
	   end

  (* interpreter for varlargs *)
    fun genInterp (largs, largsReg, endOfLargs) = [
            T.DEFINE interpLab,
	  (* loop through the largs *)
	    T.MV (defTy, largsReg, T.ADD (defTy, largs, lit' Consts.locdArgSzB)),
	    T.DEFINE interpEntryLab,
	    T.BCC (T.CMP(defTy, T.GE, largs, endOfLargs), gotoCLab)
          ]

    fun gen {interpFunPtr, largsReg, endOfLargs} = let           
	    val largs = T.REG (defTy, largsReg)
	    val (entryInstr, kindInstrs, widthInstrs, narrowingInstrs, locInstrs) = allInstrs largs
            in
	      List.concat [
	         [T.JMP (T.LABEL interpEntryLab, [])],
	         genInterp(largs, largsReg, endOfLargs),
		 genHandlers(ENTRY, entry, [entryInstr]),
		 genHandlers(KIND, kind, kindInstrs), 
		 genHandlers(WIDTH, width, widthInstrs), 
		 genHandlers(NARROWING, narrowing, narrowingInstrs),
		 genHandlers(LOC, loc, locInstrs), 
		 genCallC interpFunPtr
	      ]
	    end

  end (* GenFn *)
                                                                                                           c-call/gen/c-call-fn.sml                                                                            0000644 0000765 0000765 00000017464 11044706350 015153  0                                                                                                    ustar   mrainey                         mrainey                                                                                                                                                                                                                functor CCallFn (
    structure T : MLTREE
    structure C : CELLS
    val offSp : T.I.machine_int -> T.rexp
    val wordTy : int
		 
    structure SA : STAGED_ALLOCATION
          where type reg_id = T.reg
          where type loc_kind = CLocKind.loc_kind

  ) = struct

    structure K = CLocKind

    fun concatMap f ls = List.concat(List.map f ls)

    datatype c_arg 
      = ARG of T.rexp	
	  (* rexp specifies integer or pointer; if the 
           * corresponding parameter is a C struct, then 
	   * this argument is the address of the struct. 
	   *)
      | FARG of T.fexp
	  (* fexp specifies floating-point argument *)

    fun copyToReg (mty, r, e) = let
	val tmp = C.newReg ()
        in
	    [T.COPY (mty, [r], [tmp]), T.MV (mty, tmp, e)]
        end

    fun copyToFReg (mty, r, e) = let
	val tmp = C.newFreg ()
        in
	    [T.FCOPY (mty, [r], [tmp]), T.FMV (mty, tmp, e)] 
        end

    val stack = T.Region.stack

    fun litInt i = T.I.fromInt(wordTy, i)
    val lit = T.LI o litInt
    val offSp = offSp o litInt

  (* returns any general-purpose register IDs used in a machine location *)
    fun gprsOfLoc (SA.REG (_, K.GPR, r)) = [r]
      | gprsOfLoc (SA.COMBINE (l1, l2)) = gprsOfLoc l1 @ gprsOfLoc l2
      | gprsOfLoc (SA.NARROW (l, _, K.GPR)) = gprsOfLoc l
      | gprsOfLoc _ = []

  (* returns any floating-point register IDs used in a machine location *)
    fun fprsOfLoc (SA.REG (w, K.FPR, r)) = [(w, r)]
      | fprsOfLoc (SA.COMBINE (l1, l2)) = fprsOfLoc l1 @ fprsOfLoc l2
      | fprsOfLoc (SA.NARROW (l, _, K.FPR)) = fprsOfLoc l
      | fprsOfLoc _ = []

  (* eliminate redundant narrows, i.e., narrow.32(r1.32) == r1.32 *)
    fun elimNarrow (loc as SA.NARROW(SA.REG(wr, kr, r), wn, kn)) =
	  if kr = kn andalso wr = wn
	     then SA.REG(wr, kr, r)
	  else loc
      | elimNarrow (loc as SA.NARROW(SA.BLOCK_OFFSET(wb, kb, offset), wn, kn)) =
	  if kb = kn andalso wb = wn
	     then SA.BLOCK_OFFSET(wb, kb, offset)
	  else loc
      | elimNarrow (SA.COMBINE(l1, l2)) = SA.COMBINE(elimNarrow l1, elimNarrow l2)
      | elimNarrow loc = loc

    (* write a C argument (non aggregate) to a machine location
     *   - arg is the argument data
     *   - off is an offset into the argument data
     *   - loc is the machine location
     *   - stms is the accumulator of machine instructions
     *)
    fun writeLoc arg (off, loc, stms) = (
	  case (arg, loc)
	   of (ARG (e as T.REG _), SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset)) =>
	      (* register to stack (gpr) *)
	      T.STORE(wordTy, offSp offset, e, stack) :: stms
	    | (ARG (e as T.REG _), SA.NARROW(SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset), w', (K.GPR | K.STK))) =>
	      (* register to stack with width conversion (gpr) *)
	      T.STORE(w, offSp offset, T.SX(w, w', e), stack) :: stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.REG (w, K.GPR, r)) =>
	      (* memory to register (gpr) *)
	      copyToReg(w, r, T.LOAD (ty, T.ADD(wordTy, e, off), rgn)) @ stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.NARROW(SA.REG (w, K.GPR, r), w', K.GPR)) =>
	      (* memory to register with conversion (gpr) *)
	      copyToReg(w, r, T.SX(w, w', T.LOAD (w', T.ADD(wordTy, e, off), rgn))) @ stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset)) => let
	      (* memory to stack (gpr) *)
		val tmp = C.newReg ()
	        in
		  T.STORE (ty, offSp offset, T.REG (ty, tmp), stack) :: 
		  T.MV (ty, tmp, T.LOAD (ty, T.ADD(wordTy, e, off), rgn)) :: stms
	        end
	    | (ARG (T.LOAD (ty, e, rgn)), SA.NARROW(SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset), w', K.GPR)) => let
	      (* memory to stack with conversion (gpr) *)
		val tmp = C.newReg ()
	        in
		  T.STORE (w, offSp offset, T.REG (w, tmp), stack) :: 
		  T.MV (w, tmp, T.SX(w, w', T.LOAD (w', T.ADD(wordTy, e, off), rgn))) :: stms
	        end
	    | (ARG e, SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset)) => let
	      (* expression to stack (gpr) *)
		val tmp = C.newReg ()
	        in
		  T.STORE (w, offSp offset, T.REG (w, tmp), stack) :: T.MV (w, tmp, e) :: stms
	        end
	    | (ARG e, SA.NARROW(SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset), w', K.GPR)) => let
	      (* expression to stack with conversion (gpr) *)
		val tmp = C.newReg ()
	        in
		  T.STORE (w, offSp offset, T.REG (w, tmp), stack) :: T.MV (w, tmp, T.SX(w, w', e)) :: stms
	        end
	    | (FARG (e as T.FREG _), SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset)) =>
	      (* register to stack (fpr) *)
	      T.FSTORE (w, offSp offset, e, stack) :: stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.REG(w, K.FPR, r)) =>
	      (* memory to register (fpr) *)
	      copyToFReg(w, r, T.FLOAD (ty, T.ADD(wordTy, e, off), rgn)) @ stms
	    | (FARG (T.FLOAD (ty, e, rgn)), SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset)) => let
              (* memory to stack (fpr) *)
		val tmp = C.newFreg ()
	        in
		  T.FSTORE (w, offSp offset, T.FREG (w, tmp), stack) :: 
		  T.FMV (w, tmp, T.FLOAD (ty, T.ADD(wordTy, e, off), rgn)) :: stms
	        end
	    | (FARG e, SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset)) => let
              (* expression to stack (fpr) *)
		val tmp = C.newFreg ()
	        in
		  T.FSTORE (w, offSp offset, T.FREG (w, tmp), stack) :: T.FMV (w, tmp, e) :: stms
	        end
	    | (FARG e, SA.REG(w, K.FPR, r)) => 
	      (* expression to register (fpr) *)
	      copyToFReg(w, r, e) @ stms
	    | _ => raise Fail "invalid arg / loc pair"
          (* end case *))

  (* write a C argument (possibly an aggregate) to some parameter locations *)
    fun writeLocs' (arg, locs, stms) = let
	  val locs = List.map elimNarrow locs
        (* offsets of the members of the struct *)
	  val membOffs = List.tabulate(List.length locs, fn i => lit(i*8))
          in
	     ListPair.foldl (writeLoc arg) stms (membOffs, locs)
          end

  (* write C arguments to parameter locations; also return any used GPRs or FPRs *)
    fun writeLocs (args, argLocs) = let
	  val gprs = concatMap gprsOfLoc (List.concat argLocs)
	  val fprs = concatMap fprsOfLoc (List.concat argLocs)
	  val instrs = ListPair.foldl writeLocs' [] (args, argLocs)
          in
	     (List.rev instrs, gprs, fprs)
          end

  (* read from a machine location *)
    fun readLoc (loc, (resultRegs, copyResult)) = (
	  case elimNarrow loc
	   of SA.REG(w, K.GPR, r) => let
                (* register (gpr) *)
		val tmpR = C.newReg()
	        in
		  (T.GPR(T.REG(w, tmpR)) :: resultRegs, T.COPY(w, [tmpR], [r]) :: copyResult)
	        end
	    | SA.NARROW(loc, w', K.GPR) => let
                (* conversion (gpr) *)
		val ([resultReg as T.GPR(T.REG(_, tmp))], copyResult') = readLoc(loc, ([], []))
		val w = SA.width loc
	        in
		  (resultReg :: resultRegs, T.MV(w, tmp, T.ZX(w, w', T.REG (w', tmp))) :: copyResult' @ copyResult)
	        end
	    | SA.REG(w, K.FPR, r) => let
		val resReg = C.newFreg()
	        in
		   (T.FPR(T.FREG(w, resReg)) :: resultRegs, T.FCOPY(w, [resReg], [r]) :: copyResult)
	        end
	    | SA.NARROW(loc, w', K.FPR) => let
                (* conversion (fpr) *)
		val ([resultReg as T.FPR(T.FREG(_, tmp))], copyResult') = readLoc(loc, ([], []))
		val w = SA.width loc
	        in
		   (resultReg :: resultRegs, T.FMV(w', tmp, T.CVTF2F(w', w, T.FREG(w, tmp))) :: copyResult' @ copyResult)
	        end
	    | SA.COMBINE (l1, l2) => (
	        case (readLoc (l1, ([], [])), readLoc (l2, ([], [])))
		 of ( ([T.GPR e1], instrs1), ([T.GPR e2], instrs2) ) => let
			val w = SA.width loc
			val w' = SA.width l2
			val tmp = C.newReg()
		        in
			   (T.GPR(T.REG(w, tmp)) :: resultRegs, T.MV(w, tmp, T.ADD(w, T.SLL(w, lit w', e1), e2)) :: instrs1 @ instrs2 @ copyResult)
			end
 	        (* end case *))
	    | _ => raise Fail "bogus read location"
         (* end case *))

  (* read from some machine locations *)
    fun readLocs locs = let
	  val (resultRegs, copyResult) = List.foldl readLoc ([], []) locs
          in
	      (List.rev resultRegs, List.rev copyResult)
	  end

  end (* CCallFn *)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            