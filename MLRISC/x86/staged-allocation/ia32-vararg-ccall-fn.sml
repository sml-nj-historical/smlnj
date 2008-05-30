(* ia32-vararg-ccall-fn.sml
 *
 * C calling conventions for the X86. We use the technique of Staged Allocation (see
 * MLRISC/staged-allocation).
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor IA32VarargCCallFn (
    structure T : MLTREE
    val abi : string
    val ix : (T.stm,T.rexp,T.fexp,T.ccexp) X86InstrExt.sext -> T.sext
  (* Note that the fast_loating_point flag must match the one passed
   * to the code generator module.
   *)
    val fast_floating_point : bool ref
  ) = struct

    structure T = T
    structure C = X86Cells
    structure CB = CellsBasis
    structure CTy = CTypes
    structure CCall = IA32SVIDFn(
                       structure T = T
		       val abi = abi
		       val ix = ix
		       val fast_floating_point = fast_floating_point
		     )
    structure VarargCCall = VarargCCallFn(
			      structure T = T
			      structure CCall = CCall
			      val gprParams = []
			      val fprParams = []
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

    fun genVarargs (cFun, args) = VarargCCall.genVarargs(cFun, args)

  end
