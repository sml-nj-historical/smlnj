(* sparccomp-instr-ext.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * compiling a trivial extensions to the Sparc instruction set
 * (UNIMP instruction)
 *)
signature SPARCCOMP_INSTR_EXT = sig
    structure T : MLTREE
    structure I : SPARCINSTR

    type reducer =
	 (I.instruction, I.C.cellset, I.operand, I.addressing_mode) T.reducer

    val compileSext :
	reducer
	-> { stm: (T.stm, T.rexp, T.fexp, T.ccexp) SparcInstrExt.sext,
	     an: T.an list }
	-> unit
end

functor SparcCompInstrExt (I: SPARCINSTR) : SPARCCOMP_INSTR_EXT = struct
    structure T = I.T
    structure I = I
    structure C = I.C
    structure X = SparcInstrExt

    type stm = (T.stm, T.rexp, T.fexp, T.ccexp) X.sext

    type reducer =
	 (I.instruction, I.C.cellset, I.operand, I.addressing_mode) T.reducer

    fun compileSext reducer { stm: stm, an: T.an list } = let
	val T.REDUCER { emit, ... } = reducer
    in
	case stm of X.UNIMP i => emit (I.UNIMP {const22 = i}, an)
    end
end
