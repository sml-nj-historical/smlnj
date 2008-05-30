(* amd64-vararg-ccall-fn.sml
 *
 * C calling conventions for the AMD64. We use the technique of Staged Allocation (see
 * MLRISC/staged-allocation).
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor AMD64VarargCCallFn (
    structure T : MLTREE
  ) = struct

    structure T = T
    structure C = AMD64Cells
    structure CB = CellsBasis
    structure CTy = CTypes
    structure CCall = AMD64SVIDFn(structure T = T)
    structure VarargCCall = VarargCCallFn(
			      structure T = T
			      structure CCall = CCall
			      val gprParams = List.map #2 CCall.CCs.gprParams
			      val fprParams = List.map #2 CCall.CCs.fprParams
			      val spReg = CCall.spReg
			      val wordTy = 64
			      val newReg = C.newReg
			    )

    val wordTy = 64
    fun lit i = T.LI (T.I.fromInt (wordTy, i))

    fun callWithArgs (cFun, args) = let
	   val triplets = VarargCCall.encodeArgs args
	   in
	      raise Fail "jump to the interpreter"
	   end

    fun genVarargs (cFun, args) = 
	    T.MV(wordTy, C.rax, lit (List.length CCall.CCs.fprParams)) :: VarargCCall.genVarargs(cFun, args)

  end (* AMD64VarargCCallFn *)
