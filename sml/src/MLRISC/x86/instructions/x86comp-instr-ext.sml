(* x86comp-instr-ext.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * emit code for extensions to the x86 instruction set.
 *)

signature X86COMP_INSTR_EXT = sig
  structure I : X86INSTR
  structure CFG : CONTROL_FLOW_GRAPH where I = I

  type reducer = 
    (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) I.T.reducer

  val compileSext : 
     reducer 
      -> {stm: (I.T.stm, I.T.rexp, I.T.fexp, I.T.ccexp) X86InstrExt.sext, 
	  an: I.T.an list} 
        -> unit
end


functor X86CompInstrExt
  (structure I : X86INSTR
   structure CFG : CONTROL_FLOW_GRAPH where I = I
 ) : X86COMP_INSTR_EXT = 
struct
  structure CFG = CFG
  structure T = I.T
  structure I = I
  structure C = I.C
  structure X = X86InstrExt

  type stm = (T.stm, T.rexp, T.fexp, T.ccexp) X.sext

  type reducer = 
    (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) T.reducer

  val esp = C.esp
  val espOpnd = I.Direct(esp)

  fun error msg = MLRiscErrorMsg.error("X86CompInstrExt", msg)

  val stackArea = I.Region.stack

  fun compileSext reducer {stm: stm, an:T.an list} = let
    val T.REDUCER{operand, emit, reduceFexp, instrStream, reduceOperand,
                  ...} = reducer
    val T.Stream.STREAM{emit=emitI, ...} = instrStream
    fun fstp(sz, fstpInstr, fexp) = 
      (case fexp
        of T.FREG(sz', f) =>
	    if sz <> sz' then error "fstp: sz"
	    else emitI(fstpInstr(I.FDirect f))
         | _ => error "fstp: fexp"
      (*esac*))
  in
    case stm
    of X.PUSHL(rexp) => emit(I.PUSHL(operand rexp), an)
     | X.POP(rexp)   => emit(I.POP(operand rexp), an)

     | X.FSTPS(fexp) => fstp(32, I.FSTPS, fexp)
     | X.FSTPL(fexp) => fstp(64, I.FSTPL, fexp)
     | X.FSTPT(fexp) => fstp(80, I.FSTPT, fexp)

     | X.LEAVE	     => emit(I.LEAVE, an)
     | X.RET(rexp)   => emit(I.RET(SOME(operand rexp)), an)
     | X.LOCK_CMPXCHGL(src, dst) =>
       (* src must in a register *)
       emit(I.CMPXCHG{lock=true,sz=I.I32, 
                      src=I.Direct(reduceOperand(operand src)), 
                      dst=operand dst},an)
  end
end
