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
    structure SVID = AMD64SVIDFn(structure T = T)
    structure VarargCCall = VarargCCallFn(
			      structure T = T
			      structure CCall = SVID
			      val gprParams = List.map #2 SVID.CCs.gprParams
			      val fprParams = List.map #2 SVID.CCs.fprParams
			      val spReg = SVID.spReg
			      val wordTy = 64
			      val newReg = C.newReg
			    )
    structure SA = SVID.SA

    val wordTy = 64
    fun lit i = T.LI (T.I.fromInt (wordTy, i))
    val regToInt = CB.physicalRegisterNum

  (* one step of staged allocation *)
    fun allocateArg step (arg, (str, locs)) = let
	   val slot = SVID.slotOfCTy(VarargCCall.argToCTy arg)
	   val (str', [loc]) = SA.doStagedAllocation(str, step, [slot])
           in
	     (str', loc :: locs)
	   end

    fun encodeLoc (_, SA.REG (_, r), SVID.K_GPR) = (VarargCCall.GPR, regToInt r)
      | encodeLoc (_, SA.REG (_, r), SVID.K_FPR) = (VarargCCall.FPR, regToInt r)
      | encodeLoc (_, SA.BLOCK_OFFSET offB, SVID.K_GPR) = (VarargCCall.STK, offB)
      | encodeLoc (_, SA.BLOCK_OFFSET offB, SVID.K_FPR) = (VarargCCall.STK, offB)
      | encodeLoc (_, SA.NARROW (loc, w', k), _) = encodeLoc (w', loc, k)

  (* takes a vararg and a location and returns the vararg triplet *)
    fun varArgTriplet (arg, loc) = let
	   val (k, l) = encodeLoc loc
           in
	     (arg, k, l)
	   end

  (* takes a list of varargs and returns vararg triplets *)
    fun encodeArgs args = let
	   val step = SA.mkStep SVID.CCs.callStages
	   val (str, locs) = List.foldl (allocateArg step) (SVID.CCs.str0, []) args
           in
	      ListPair.mapEq varArgTriplet (args, List.rev locs)
	   end

    fun callWithArgs (cFun, args) = let
	   val triplets = encodeArgs args
	   in
	      raise Fail "jump to the interpreter"
	   end

    fun genVarargs (cFun, args) = 
	    T.MV(wordTy, C.rax, lit (List.length SVID.CCs.fprParams)) :: VarargCCall.genVarargs(cFun, args)

  end (* AMD64VarargCCallFn *)
