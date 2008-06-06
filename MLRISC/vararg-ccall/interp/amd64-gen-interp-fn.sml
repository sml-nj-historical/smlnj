(* amd64-vararg-ccall-fn.sml
 *
 * C calling conventions for the AMD64. We use the technique of Staged Allocation (see
 * MLRISC/staged-allocation).
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor AMD64GenInterpFn (
    structure T : MLTREE
    val push : T.rexp -> T.stm
    val leave : T.stm
  ) = struct

    structure T = T
    structure C = AMD64Cells
    structure CB = CellsBasis
    structure CTy = CTypes
    structure CCall = AMD64SVIDFn(structure T = T)
    structure GenInterp = GenInterpFn(
			      structure T = T
			      val callerSaveRegs = CCall.callerSaveRegs
			      val callerSaveFRegs = CCall.callerSaveFRegs
			      val gprParams = List.map #2 ((64, C.rax) :: CCall.CCs.gprParams)
			      val fprParams = List.map #2 CCall.CCs.fprParams
			      val gprTys = [32, 64]
			      val fprTys = [32, 64]
			      val spReg = CCall.spReg
			      val wordTy = 64
			      val newReg = C.newReg
			    )

    val wordTy = 64
    fun lit i = T.LI (T.I.fromInt (wordTy, i))

    fun callWithArgs (cFun, args) = let
	   in
	      raise Fail "jump to the interpreter"
	   end

    fun genVarargs () = let
	   val lab = Label.global "varargs"
	   val args = C.newReg()
	   val cFun = C.newReg()
	   val endOfArgs = raise Fail "todo"
           in
	       (lab,
		List.concat [
                 (* the abi specifies that rax contains the number of floating-point arguments *)
	           [T.MV(wordTy, C.rax, lit (List.length CCall.CCs.fprParams))],
		   [push (T.REG(64, C.rbp)),
		    T.COPY (wordTy, [C.rbp], [C.rsp])],		   
		   [T.MV(wordTy, cFun, T.REG(wordTy, C.rdi))],    (* arg0 *)
		   [T.MV(wordTy, args, T.REG(wordTy, C.rsi))],    (* arg1 *)
	           GenInterp.genVarargs(T.REG(wordTy, cFun), args, endOfArgs),
		   [leave],
		   [T.RET []]
		   ])
	   end

  end (* AMD64VarargCCallFn *)
