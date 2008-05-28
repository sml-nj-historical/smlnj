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
    structure SVID = SVIDFn(structure T = T)
    structure CCall = SVID.CCall
    structure SA = SVID.SA

    datatype argument = I of int | R of real | B of bool | S of string

    val wordTy = 64
    val mem = T.Region.memory
    val stack = T.Region.stack

    fun lit i = T.LI (T.I.fromInt (wordTy, i))

    val GPR = 0
    val FPR = 1
    val STK = 2

    fun argToCTy (I _) = CTy.C_signed CTy.I_int
      | argToCTy (R _) = CTy.C_double
      | argToCTy (B _) = CTy.C_signed CTy.I_int
      | argToCTy (S _) = CTy.C_PTR

  (* one step of staged allocation *)
    fun allocateArg step (arg, (str, locs)) = let
	   val slot = SVID.slotOfCTy(argToCTy arg)
	   val (str', [loc]) = SA.doStagedAllocation(str, step, [slot])
           in
	     (str', loc :: locs)
	   end

    fun encodeLoc (_, SA.REG (_, r), SVID.K_GPR) = (GPR, CB.physicalRegisterNum r)
      | encodeLoc (_, SA.REG (_, r), SVID.K_FPR) = (FPR, CB.physicalRegisterNum r)
      | encodeLoc (_, SA.BLOCK_OFFSET offB, SVID.K_GPR) = (STK, offB)
      | encodeLoc (_, SA.BLOCK_OFFSET offB, SVID.K_FPR) = (STK, offB)

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

  end (* AMD64VarargCCallFn *)
