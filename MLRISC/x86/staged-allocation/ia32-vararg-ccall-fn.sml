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
    val push : T.rexp -> T.stm
    val leave : T.stm
  ) = struct

    val wordTy = 32

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
			      val gprTys = [32]
			      val fprTys = [32, 64]
			      val spReg = CCall.spReg
			      val wordTy = wordTy
			      val newReg = C.newReg
			    )

    fun lit i = T.LI (T.I.fromInt (wordTy, i))

  (* get the ith argument in the calling sequence *)
    fun getArg i = 
	    T.LOAD(wordTy, T.ADD(wordTy, T.REG(wordTy, C.ebp), lit (4*i+8)), T.Region.memory)

    fun genVarargs () = let
  	   val lab = Label.global "varargs"
	   val args = C.newReg()
	   val cFun = C.newReg()
        (* we align the frame to a 16-bytes to support Mac OS. *)
	   val frameSzB = 1024*4-2*4
	   val endOfArgs = raise Fail "todo"
           in
	      (lab,
	       List.concat [
	           [T.LIVE CCall.CCs.calleeSaveRegs],
		   [push (T.REG(wordTy, C.ebp)),
		    T.COPY (wordTy, [C.ebp], [C.esp])],
		   [T.MV(wordTy, cFun, getArg 0)],
		   [T.MV(wordTy, args, getArg 1)],
		 (* allocate stack space for the arguments *)
		   [T.MV(wordTy, C.esp, T.SUB(wordTy, T.REG(wordTy, C.esp), lit frameSzB))],
	           VarargCCall.genVarargs (T.REG(wordTy, cFun), args, endOfArgs),
		   [leave],
	           [T.LIVE CCall.CCs.calleeSaveRegs],
		   [T.RET []]
		   ])
	   end	    

  end
