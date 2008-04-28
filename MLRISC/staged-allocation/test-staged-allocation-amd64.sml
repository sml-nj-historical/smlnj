val floats16ByteAligned = true

structure AMD64MLTree =
   MLTreeF (structure Constant  = UserConst
    structure Region    = UserRegion
    structure Extension = UserExtension)

structure AMD64MLTreeEval =
   MLTreeEval (structure T = AMD64MLTree
    fun eq _ _ = false
    val eqRext = eq val eqFext = eq
    val eqCCext = eq val eqSext = eq)

(*
structure AMD64PseudoOps  =
  struct
    structure Client =
      struct
        datatype pseudo_op_ext = COMM of (Label.label * int)
        structure AsmPseudoOps = AMD64GasPseudoOps (
			     structure T = AMD64MLTree
			     structure MLTreeEval = AMD64MLTreeEval)
        type pseudo_op = pseudo_op_ext        
        fun toString (COMM(lab, sz)) = concat[         
            "\t.comm\t"(*, P.lexpToString(P.T.LABEL lab)*),
            ",", Int.toString sz]
        fun emitValue {pOp, loc, emit} = raise Fail "emitValue"
        fun sizeOf _ = 0
        fun adjustLabels _ = false
      end (* Client *)

    structure T = AMD64MLTree
    type pseudo_op = (T.labexp, Client.pseudo_op) PseudoOpsBasisTyp.pseudo_op
    fun toString _ = ""
    fun emitValue _ = ()
    fun sizeOf _ = 0
    fun adjustLabels _ = false
end (* AMD64PseudoOps *)
*)


functor AMD64PseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = AMD64GasPseudoOps (
    structure T = T
    structure MLTreeEval = MLTreeEval)

(*
functor AMD64PseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = AMD64DarwinPseudoOps (
    structure T = T
    structure MLTreeEval = MLTreeEval)
*)

structure AMD64PseudoOps = AMD64PseudoOpsFn(
            structure T = AMD64MLTree
            structure MLTreeEval = AMD64MLTreeEval)

structure PseudoOps =
  struct

    structure Client =
      struct
	structure AsmPseudoOps = AMD64PseudoOps
	type pseudo_op = unit
			 
	fun toString () = ""
  
	fun emitValue _ = raise Fail "todo"
	fun sizeOf _ = raise Fail "todo"
	fun adjustLabels _ = raise Fail "todo"
      end (* Client *)
  
    structure PseudoOps = PseudoOps (structure Client = Client)
  end

structure AMD64Stream = InstructionStream(PseudoOps.PseudoOps)
structure AMD64Instr = AMD64Instr (AMD64MLTree)
structure AMD64Shuffle = AMD64Shuffle(AMD64Instr)

structure AMD64MLTreeHash =
   MLTreeHash (structure T = AMD64MLTree
    fun h _ _ = 0w0
    val hashRext = h val hashFext = h
    val hashCCext = h val hashSext = h)

structure AMD64Asm = AMD64AsmEmitter
   (structure Instr = AMD64Instr
    structure S = AMD64Stream
    structure MLTreeEval = AMD64MLTreeEval
    structure Shuffle = AMD64Shuffle
   )

structure AMD64InsnProps = AMD64Props 
			  (structure Instr = AMD64Instr
                           structure MLTreeHash = AMD64MLTreeHash
			   structure MLTreeEval = AMD64MLTreeEval)

structure AMD64CFG = ControlFlowGraph (
            structure I = AMD64Asm.I
	    structure GraphImpl = DirectedGraph
	    structure InsnProps = AMD64InsnProps
	    structure Asm = AMD64Asm)

(*structure AMD64Stream = InstructionStream(AMD64PseudoOps)*)
structure AMD64MLTStream = MLTreeStream (
		      structure T = AMD64MLTree
		      structure S = AMD64Stream)

structure CompInstrExt = AMD64CompInstrExt (
      structure I = AMD64Instr
      structure TS = AMD64MLTStream
      structure CFG = AMD64CFG)

structure AMD64MTC = struct
  structure T = AMD64MLTree
  structure TS = AMD64MLTStream
  structure I = AMD64Instr
  structure CFG = AMD64CFG
  structure C = I.C
   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode,AMD64CFG.cfg) TS.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "UserMLTreeExtComp"
   val compileSext  = CompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
		      
   structure AMD64MLTreeUtils : MLTREE_UTILS =
     struct
       structure T = AMD64MLTree
       structure IX = AMD64InstrExt
       structure U = MLTreeUtils (
       structure T = T
       fun hashSext _ _ = 0w0
       fun hashRext _ _ = 0w0
       fun hashFext _ _ = 0w0
       fun hashCCext _ _ = 0w0
       fun eqSext _ _ = raise Fail "eqSext"
       fun eqRext _ _ = raise Fail "eqRext"
       fun eqFext _ _ = raise Fail "eqFext"
       fun eqCCext _ _ = raise Fail "eqCCext"
       fun showSext (prt : T.printer) ext = raise Fail "todo"
       fun showRext _ _ = raise Fail "showRext"
       fun showFext _ _ = raise Fail "showFext"
       fun showCCext _ _ = raise Fail "showCCext")
       open U
     end
end

structure AMD64 = AMD64Gen (
		  structure I = AMD64Instr
		  structure MLTreeUtils = AMD64MTC.AMD64MLTreeUtils
		  structure ExtensionComp = AMD64MTC
		  val floats16ByteAligned = floats16ByteAligned
		  fun signBit _ = raise Fail "todo"
		  fun negateSignBit _ = raise Fail "todo"
		  )

structure AMD64Emit = CFGEmit (
             structure CFG = AMD64CFG
             structure E = AMD64Asm) 


structure AMD64FlowGraph = BuildFlowgraph 
	    (structure Props = AMD64InsnProps
             structure Stream = AMD64Stream
	     structure CFG = AMD64CFG)

structure AMD64Expand = CFGExpandCopies (structure CFG=AMD64CFG
                                         structure Shuffle = AMD64Shuffle)
structure AMD64BlockPlacement = DefaultBlockPlacement(AMD64CFG)

structure RASpill = RASpillWithRenaming (
    structure Asm = AMD64Asm
    structure InsnProps = AMD64InsnProps
    val max_dist = ref 4
    val keep_multiple_values = ref false)

structure C = AMD64Cells

datatype spill_operand_kind = SPILL_LOC 
                            | CONST_VAL

datatype ra_phase = SPILL_PROPAGATION 
                  | SPILL_COLORING

structure IntRA = 
  struct
    val dedicated = [C.rsp, C.rbp]
    val allRegs = C.Regs CellsBasis.GP {from=0, to=15, step=1}
    val allRegsSet = foldl C.addReg C.empty allRegs
    val avail = let
        val availSet = foldl C.rmvReg allRegsSet dedicated
        in
          C.getReg availSet
        end
    fun spillInit _ = ()
    fun spillLoc {info=frame, an, cell, id=loc} = 
raise Fail ""
(*        {opnd = AMD64Instr.Immed 0, kind = SPILL_LOC}*)
    val phases = [SPILL_PROPAGATION, SPILL_COLORING]
  end (* IntRA *)

structure FloatRA =
  struct
    val avail = C.Regs CellsBasis.FP {from=0, to=15, step=1}
    val dedicated = []
    fun spillInit _ = ()
    fun spillLoc (info, ans, id) = raise Fail ""
    val phases = [SPILL_PROPAGATION, SPILL_COLORING]
  end (* FloatRA *)

(* register allocation *)
structure AMD64RA = AMD64RegAlloc (
         structure I = AMD64Instr
         structure CFG = AMD64CFG
         structure Asm = AMD64Asm
         structure SpillHeur = ChowHennessySpillHeur
         structure Spill = RASpill
         structure Props = AMD64InsnProps
	 val floats16ByteAligned = floats16ByteAligned
         type spill_info = unit
         fun beforeRA (Graph.GRAPH graph) = ()
         datatype spill_operand_kind = datatype spill_operand_kind
         datatype ra_phase = datatype ra_phase
         structure Int = IntRA
         structure Float = FloatRA)

structure AMD64Expand = CFGExpandCopies (
    structure CFG=AMD64CFG
    structure Shuffle = AMD64Shuffle)

structure CCalls = AMD64SVID (
           structure T = AMD64MLTree
           val frameAlign = 16)


structure RA2 = 
    RISC_RA
    (structure I = AMD64Instr
     structure Asm = AMD64Asm
     structure CFG = AMD64CFG
     structure InsnProps = AMD64InsnProps
     structure Rewrite = 
       struct
         structure I = AMD64Instr
	 structure C=I.C
	 structure CB = CellsBasis
	 fun error msg = MLRiscErrorMsg.error("X86Rewrite", msg)
			 
	 fun operand (rs,rt) opnd =
	     (case opnd
	       of I.Direct (sz, r) => if CB.sameColor(r,rs) then I.Direct (sz, rt) else opnd
		| I.Displace{base, disp, mem} => 
		  if CB.sameColor(base,rs) then I.Displace{base=rt, disp=disp, mem=mem} 
		  else opnd
		| I.Indexed{base as SOME b, index, scale, disp, mem} => let
		      val base'= if CB.sameColor(b,rs) then SOME rt else base
		      val index'=if CB.sameColor(index,rs) then rt else index
		  in I.Indexed{base=base', index=index', scale=scale, disp=disp, mem=mem}
		  end
		| I.Indexed{base, index, scale, disp, mem=mem}  => 
		  if CB.sameColor(index,rs) then 
		      I.Indexed{base=base, index=rt, scale=scale, disp=disp, mem=mem}
		  else opnd
		| _ => opnd
              (*end case*))
	     

	 fun rewriteDef (instr, rs, rt) = let
	     fun operand(opnd as I.Direct (sz, r)) = 
		 if CB.sameColor(r,rs) then I.Direct (sz, rt) else opnd
	       | operand _ = error "operand: not I.Direct"
	     fun replace r = if CB.sameColor(r,rs) then rt else r
	     fun rewriteX86Def(instr) =
		 (case instr 
		   of I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} => 
		      I.CALL{opnd=opnd, cutsTo=cutsTo, 
			     return=CB.CellSet.map {from=rs,to=rt} return, pops=pops,
			     defs=CB.CellSet.map {from=rs,to=rt} defs, uses=uses, mem=mem}
		    | I.MOVE{mvOp, src, dst} => I.MOVE{mvOp=mvOp, src=src, dst=operand dst}
		    | I.LEAL{r32, addr} => I.LEAL{r32=replace r32, addr=addr}
		    | I.LEAQ{r64, addr} => I.LEAQ{r64=replace r64, addr=addr}
		    | I.BINARY{binOp, src, dst} => 
		      I.BINARY{binOp=binOp, src=src, dst=operand dst}
		    | I.SHIFT{shiftOp, src, dst, count} => 
		      I.SHIFT{shiftOp=shiftOp, src=src, count=count, dst=operand dst}
		    | I.UNARY{unOp, opnd} => I.UNARY{unOp=unOp, opnd=operand opnd}
		    | I.SET{cond, opnd} => I.SET{cond=cond, opnd=operand opnd}
		    | _ => instr
	        (* end case *))

	     fun f (I.ANNOTATION{a,i}) =
		 I.ANNOTATION{i=rewriteDef(i,rs,rt),
			      a=(case a of
				     CB.DEF_USE{cellkind=CB.GP,defs,uses} =>
			             CB.DEF_USE{cellkind=CB.GP,uses=uses,
				 		defs=map replace defs}
				   | _ => a)}
	       | f (I.INSTR i) = I.INSTR(rewriteX86Def(i))
	       | f (I.COPY{k as CB.GP, sz, dst, src, tmp}) =
		 I.COPY{k=k, sz=sz, dst=map replace dst, src=src, tmp=tmp}
	 in 
	     f(instr)
	 end


	 fun rewriteUse (instr, rs, rt) = let
	     val operand = operand (rs, rt)
	     fun replace r = if CB.sameColor(r,rs) then rt else r
	     fun rewrite instr = (case instr
                 of I.JMP(opnd, labs) => I.JMP(operand opnd, labs)
		  | I.JCC{cond, opnd} => I.JCC{cond=cond, opnd = operand opnd}
		  | I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} => 
		    I.CALL{opnd=operand opnd, defs=defs, return=return,
			   uses=CB.CellSet.map {from=rs,to=rt} uses, cutsTo=cutsTo,
			   mem=mem, pops=pops}
		  | I.MOVE{mvOp, src, dst as I.Direct _} => 
		    I.MOVE{mvOp=mvOp, src=operand src, dst=dst}
		  | I.MOVE{mvOp, src, dst} => 
		    I.MOVE{mvOp=mvOp, src=operand src, dst=operand dst}
		  | I.LEAL{r32, addr} => I.LEAL{r32=r32, addr=operand addr}
		  | I.LEAQ{r64, addr} => I.LEAQ{r64=r64, addr=operand addr}
		  | I.CMPL{lsrc, rsrc} => I.CMPL{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.CMPW{lsrc, rsrc} => I.CMPW{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.CMPB{lsrc, rsrc} => I.CMPB{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.TESTL{lsrc, rsrc} => I.TESTL{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.TESTW{lsrc, rsrc} => I.TESTW{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.TESTB{lsrc, rsrc} => I.TESTB{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.BITOP{bitOp, lsrc, rsrc} => 
		    I.BITOP{bitOp=bitOp, lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.BINARY{binOp, src, dst} => 
		    I.BINARY{binOp=binOp, src=operand src, dst=operand dst}
		  | I.SHIFT{shiftOp, src, dst, count} => 
		    I.SHIFT{shiftOp=shiftOp, src=operand src, dst=operand dst, 
			    count=operand src}
                (* end case *))

             fun f(I.ANNOTATION{a,i}) = 
		 I.ANNOTATION{i=rewriteUse(i, rs, rt),
			      a = case a of
				      CB.DEF_USE{cellkind=CB.GP,defs,uses} =>
				      CB.DEF_USE{cellkind=CB.GP,uses=map replace uses,
						 defs=defs}
				    | _ => a}
	       | f(I.INSTR i) = I.INSTR(rewrite(i))
	       | f(I.COPY{k as CB.GP, sz, dst, src, tmp}) = 
		 I.COPY{k=k, sz=sz, dst=dst, src=List.map replace src, tmp=tmp}
	 in 
	     f (instr:I.instruction)
	 end

      
	 fun frewriteDef _ = raise Fail ""
	 fun frewriteUse _ = raise Fail ""
       end
     structure SpillInstr = AMD64SpillInstr (
               structure I = I
               structure Props = AMD64InsnProps
	       val floats16ByteAligned = true)
     structure SpillHeur = ChaitinSpillHeur
     structure Spill = RASpill (structure InsnProps = AMD64InsnProps
                                structure Asm = AMD64Asm)
     
     datatype spillOperandKind = SPILL_LOC | CONST_VAL
     type spill_info = unit
     fun beforeRA _ = ()

     val architecture = "amd64"
     fun pure _ = true

     structure Int =
	struct
	  val avail = C.Regs CellsBasis.GP {from=0, to=15, step=1}
	  val dedicated = [C.rsp, C.rbp]
	  fun spillLoc _ = raise Fail ""
	  val mode = RACore.NO_OPTIMIZATION
	end
     structure Float =
	struct
	  val avail = C.Regs CellsBasis.FP {from=0, to=15, step=1}
	  val dedicated = []
	  fun spillLoc _ = raise Fail ""
	  val mode = Word.orb (RACore.HAS_PARALLEL_COPIES, RACore.DEAD_COPY_ELIM)
	end

    )
			   
