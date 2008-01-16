(* amd64-svid-fn.sml
 *
 * C calling conventions using staged allocation.
 *)

functor AMD64SVID (
    structure T : MLTREE
    (* alignment requirement for stack frames; should be a power of two
     * that is at least eight.
     *)
    val frameAlign : int 
  ) : C_CALL =
  struct

    structure T = T
    structure C = AMD64Cells
    structure CB = CellsBasis
    structure CTy = CTypes

    val wordTy = 64
    val stack = T.Region.stack
    val mem = T.Region.memory
    fun gpr r = T.GPR (T.REG (wordTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))

    (* GPRs *)
    val [rax, rbx, rdi, rsi, rdx, rcx, r8, r9, r10, r11, r12, r13, r14, r15] = 
	  map (fn r => (wordTy, r)) 
	    ([C.rax, C.rbx, C.rdi, C.rsi, C.rdx, C.rcx] @
	     C.Regs CB.GP {from=8, to=15, step=1})
    (* FPRs *)
    val sseFRegs as 
        [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, 
         xmm11, xmm12, xmm13, xmm14, xmm15] =
           map (fn r => (64, r)) (C.Regs CB.FP {from=0, to=15, step=1})
    val calleeSaveRegs = map #2 [rbx, r12, r13, r14, r15]
    val callerSaveRegs = map #2 [rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11]
    val callerSaveFRegs = sseFRegs
    val calleeSaveFRegs = []

    datatype c_arg 
      = ARG of T.rexp	
	  (* rexp specifies integer or pointer; if the 
           * corresponding parameter is a C struct, then 
	   * this argument is the address of the struct. 
	   *)
      | FARG of T.fexp
	  (* fexp specifies floating-point argument *)
      | ARGS of c_arg list
	  (* list of arguments corresponding to the contents of a C struct *)
    datatype location_kind = K_GPR | K_FPR | K_MEM
    structure TargetLang =
      struct
	datatype location_kind = datatype location_kind
      end
    structure StagedAllocation = StagedAllocationFn (
				  structure T = T
				  structure TargetLang = TargetLang )
    structure S = StagedAllocation

    (* This structure contains the automaton used in staged allocation. *)
    structure SVIDConventions =
      struct

	type reg = (int * CellsBasis.cell)
	type slot = S.slot
	type location_info = S.location_info
	type automaton = {s0 : S.str, step : S.stepper_fn}

	val gprParams = [rdi, rsi, rdx, rcx, r8, r9]
	val fprParams = [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7]

	val maxAlign = 16

	(* parameter-passing conventions *)
        fun call () = let
	    val cStack = S.freshCounter ()
	    val cInt = S.freshCounter ()
	    val cFloat = S.freshCounter ()
	in
	  ( cStack, [cStack, cInt, cFloat],
	    [ S.CHOICE [
	      (* GPR *)
	      (fn (w, k, str) => k = K_GPR, S.SEQ [
		 S.WIDEN (fn w => Int.max (wordTy, w)),
		 S.BITCOUNTER cInt,
		 S.REGS_BY_BITS (cInt, gprParams)] ),
	      (* FPR *)
	      (fn (w, k, str) => k = K_FPR, S.SEQ [
	         S.WIDEN (fn w => Int.max (64, w)),
	         S.BITCOUNTER cFloat,
	         S.REGS_BY_BITS (cFloat, fprParams) ]),
	      (* MEM *)
	      (fn (w, k, str) => k = K_MEM,
	         S.OVERFLOW {counter=cStack, blockDirection=S.UP, maxAlign=maxAlign}) ],
	     S.OVERFLOW {counter=cStack, blockDirection=S.UP, maxAlign=maxAlign}
	  ] )
	end (* call *)

	val gprRets = [rax, rdx]
	val fprRets = [xmm0, xmm1]

	(* value-returning conventions *)
	fun return () = let
	    val (cFloat, ssFloat) = S.useRegs fprRets
	    val (cInt, ssGpr) = S.useRegs gprRets
	in
	  ( [cFloat, cInt],
	    [ S.CHOICE [
	     (* GPR *)
	     (fn (w, k, str) => k = K_GPR,
	        S.SEQ [S.WIDEN (fn w => Int.max (wordTy, w)), ssGpr]),
	     (* FPR *)
	     (fn (w, k, str) => k = K_FPR,
	        S.SEQ [S.WIDEN (fn w => Int.max (64, w)), ssFloat]),
	     (* MEM *)
	     (fn (w, k, str) => k = K_MEM,
(* FIXME! *)
		ssGpr) ]
	       ] )
	end (* return *)

        (* For calls and returns, genAutomaton, initializes counters,
         * returns an initial store, and returns a stepper function.
	 * Calls also have a finisher function that returns the size
	 * of the argument area.
         *)
	fun genAutomaton () = let
	    val (stackCounter, callCounters, callStates) = call ()
	    val (retCounters, retStates) = return ()
	    fun finish str = S.find (str, stackCounter)
	in
	  {call = {cS0=S.init callCounters, 
		   cStep=S.mkStep callStates, finish=finish},
	   ret  = {rS0=S.init retCounters, rStep=S.mkStep retStates}}
	end (* genAutomaton *)

      end (* SVIDConventions *)

    (* An arg_location specifies the location of arguments/parameters
     * for a C call.  Offsets are given with respect to the low end 
     * of the parameter area. *)
    datatype arg_location =
	C_GPR  of (T.ty * T.reg) (* integer/pointer argument in register *)
      | C_FPR  of (T.fty * T.reg) (* floating-point argument in register *)
      | C_STK  of (T.ty * T.I.machine_int)  (* integer/pointer argument on the call stack *)
      | C_FSTK of (T.fty * T.I.machine_int) (* floating-point argument on the call stack *)

    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = K_FPR
      | kindOfCTy (CTy.C_STRUCT _ | CTy.C_UNION _ | CTy.C_ARRAY _) = K_MEM
      | kindOfCTy _ = K_GPR
    fun szToLoc cty {sz, align} = (sz * 8, kindOfCTy cty, align)
    fun cTyToLoc cty = szToLoc cty (CSizes.sizeOfTy cty)

    fun argLoc _ (w, S.REG (_, r), K_GPR) = C_GPR (w, r)
      | argLoc _ (w, S.REG (_, r), K_FPR) = C_FPR (w, r)
      | argLoc argOffset (w, S.BLOCK_OFFSET offB, K_GPR) = 
	C_STK (w, T.I.fromInt (wordTy, offB+argOffset))
      | argLoc argOffset (w, S.NARROW (loc, w', k), _) = 
	argLoc argOffset (w', loc, k)
      | argLoc _ (w, S.COMBINE _, _) = raise Fail "impossible"

   (* takes a calling convention, return type, and param types and returns locations for setting up the call *)
    fun layout {conv, retTy, paramTys} = let
	val {call={cS0, cStep, finish}, ret={rS0, rStep}} = SVIDConventions.genAutomaton ()
	(* generate locations for the return  *)
	fun rLoc () = argLoc 0 (#2 (rStep (rS0, cTyToLoc retTy)))
	val (resLoc, structRetLoc, argOffset) = (case retTy
	     of CTy.C_void => (NONE, NONE, 0)
	      | CTy.C_UNION tys => raise Fail "todo"
	      | CTy.C_STRUCT tys => let
		val {sz, align} = CSizes.sizeOfStruct tys
		in
		  (SOME (rLoc ()), SOME {szb=sz, align=align}, 8)
		end
	      | _ => (SOME (rLoc ()), NONE, 0)
	     (* end case *))
	val argLoc = argLoc argOffset
	(* generate locations for the call *)
	fun assign (str, [], locs) = (finish str, rev locs)
          | assign (str, pTy :: pTys, locs) = let
	    val (str', cLoc) = cStep (str, cTyToLoc pTy)
	    val loc = argLoc cLoc
	    in
		assign (str', pTys, loc:: locs)
	    end (* assign *)
	val (frameSz, argLocs) = assign (cS0, paramTys, [])
	val argMem = {szb=CSizes.alignAddr (frameSz, frameAlign), align=frameAlign}
	in
	  {argLocs=argLocs, resLoc=resLoc, argMem=argMem, structRetLoc=structRetLoc}
	end (* layout *)

    val spReg = T.REG (wordTy, C.rsp)

    fun genCall {name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args} = let
	val {argLocs, argMem, resLoc, structRetLoc} = layout proto
	val argAlloc = if ((#szb argMem = 0) orelse paramAlloc argMem)
			then []
			else [T.MV (wordTy, C.rsp, T.SUB (wordTy, spReg, 
			      T.LI (T.I.fromInt (wordTy, #szb argMem))))]
	val (copyArgs, gprUses, fprUses) = let
	    fun offSp 0 = spReg
	      | offSp offset = T.ADD (wordTy, spReg, T.LI offset)
	    fun f ([], [], stms, gprs, fprs) = (rev stms, gprs, fprs)
	      | f (arg :: args, loc :: locs, stms, gprs, fprs) = let
		val (stms, gprs, fprs) = (case (arg, loc)
		    of (ARG (e as T.REG _), C_STK (mty, offset)) =>
		       (T.STORE (wordTy, offSp offset, e, stack) :: stms, gprs, fprs)
		     | (ARG e, C_STK (mty, offset)) => let
		       val tmp = C.newReg ()
		       in
			 (T.STORE (mty, offSp offset, T.REG (mty, tmp), stack) ::T.MV (mty, tmp, e) :: stms, gprs, fprs)
		       end
		     | (ARG e, C_GPR (mty, r)) => let
		       val tmp = C.newReg ()
		       in
			 (T.COPY (mty, [r], [tmp]) :: T.MV (mty, tmp, e) :: stms, r :: gprs, fprs)
		       end
		     | (FARG (e as T.FREG _), C_STK (mty, offset)) =>
		       (T.FSTORE (mty, offSp offset, e, stack) :: stms, gprs, fprs)
		     | (FARG e, C_STK (mty, offset)) => let
		       val tmp = C.newFreg ()
		       in
			 (T.FSTORE (mty, offSp offset, T.FREG (mty, tmp), stack) :: T.FMV (mty, tmp, e) :: stms, gprs, fprs)
		       end
		     | (FARG e, C_FPR (mty, r)) => let
		       val tmp = C.newFreg ()
		       in
			 (T.FCOPY (mty, [r], [tmp]) :: T.FMV (mty, tmp, e) :: stms, gprs, (mty, r) :: fprs)
		       end
		     | _ => raise Fail "todo"
		    (* end case *))
		in
		  f (args, locs, stms, gprs, fprs)
		end
	      | f _ = raise Fail "argument arity error"
	    in
	      f (args, argLocs, [], [], [])
	    end
       (* the defined registers of the call depend on the calling convention *)
 	val defs = (case #conv proto
            of "ccall" => List.map gpr callerSaveRegs @ List.map fpr callerSaveFRegs
	     | "ccall-bare" => []
	     | conv => raise Fail (concat [
			"unknown calling convention \"", String.toString conv, "\""
		      ])
            (* end case *))
	val uses = List.map gpr gprUses @ List.map fpr fprUses
	val callStm = T.CALL {funct=name, targets=[], defs=defs, uses=uses, region=mem, pops=0}
	val (resultRegs, copyResult) = (case resLoc
	     of NONE => ([], [])
	      | SOME (C_GPR (ty, r)) => let
		val resReg = C.newReg ()
		in
		  ([T.GPR (T.REG (ty, resReg))],	 
		   [T.COPY (ty, [resReg], [r])])
		end
	      | SOME (C_FPR (ty, r)) => let
		val resReg = C.newFreg ()
		in
		   ([T.FPR (T.FREG (ty, resReg))],
		    [T.FCOPY (ty, [resReg], [r])])
		end
	      (* end case *))
	val callSeq = argAlloc @ copyArgs @ [callStm] @ copyResult
    in
      {callseq=callSeq, result=resultRegs}
    end (* genCall *)

  end (* AMD64SVID *)
