(* amd64-svid-fn.sml
 *
 * C calling conventions using staged allocation.
 *
 *)

functor AMD64SVID (
    structure T : MLTREE
    val frameAlign : int 
  ) (*: C_CALL*) =
  struct

    structure T = T
    structure C = AMD64Cells
    structure CB = CellsBasis
    structure CTy = CTypes

    val wordTy = 64
    val mem = T.Region.memory
    fun gpr r = T.GPR (T.REG (wordTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))

    (* general-purpose registers *)
    val [rax, rbx, rdi, rsi, rdx, rcx, r8, r9, r10, r11, r12, r13, r14, r15] = 
	  List.map (fn r => (wordTy, r)) 
	    ([C.rax, C.rbx, C.rdi, C.rsi, C.rdx, C.rcx] @
	     C.Regs CB.GP {from=8, to=15, step=1})
    (* floating-point registers (SSE2) *)
    val sseFRegs as 
        [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, 
         xmm11, xmm12, xmm13, xmm14, xmm15] =
           List.map (fn r => (64, r)) (C.Regs CB.FP {from=0, to=15, step=1})
    val calleeSaveRegs = List.map #2 [rbx, r12, r13, r14, r15]
    val callerSaveRegs = List.map #2 [rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11]
    val callerSaveFRegs = sseFRegs
    val calleeSaveFRegs = []
    val spReg = T.REG (wordTy, C.rsp)

    datatype location_kind = K_GPR | K_FPR | K_MEM
    structure S = StagedAllocationFn (
				  structure T = T
				  structure TargetLang = struct
                                        datatype location_kind = datatype location_kind
                                      end
				  val memSize = 8
				 )

    structure CCall = CCallStkFn (
				structure C = AMD64Cells
				structure T = T
				val spReg = spReg
				val wordTy = wordTy
		      )
    datatype c_arg = datatype CCall.c_arg
    datatype arg_location = datatype CCall.arg_location

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
		     (* pass in general-purpose register *)
		     (fn (w, k, str) => k = K_GPR, S.SEQ [
				  S.WIDEN (fn w => Int.max (wordTy, w)),
				  S.BITCOUNTER cInt,
				  S.REGS_BY_BITS (cInt, gprParams)] ),
		     (* pass in floating point register *)
		     (fn (w, k, str) => k = K_FPR, S.SEQ [
				  S.WIDEN (fn w => Int.max (64, w)),
				  S.BITCOUNTER cFloat,
				  S.REGS_BY_BITS (cFloat, fprParams) ]),
		     (* pass on the stack *)
		     (fn (w, k, str) => k = K_MEM,
		      S.OVERFLOW {counter=cStack, blockDirection=S.UP, maxAlign=maxAlign}) 
		     ],
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
	        (* return in general-purpose register *)
	           (fn (w, k, str) => k = K_GPR,
	            S.SEQ [S.WIDEN (fn w => Int.max (wordTy, w)), ssGpr]),
		   (* return in floating-point register *)
		   (fn (w, k, str) => k = K_FPR,
	            S.SEQ [S.WIDEN (fn w => Int.max (64, w)), ssFloat]),
		   (* return in a memory location *)
		   (fn (w, k, str) => k = K_MEM,
		    (* FIXME! *)
		    ssGpr) ]
		  ] )
	    end (* return *)

        (* generate the finite automaton for the target machine's calling conventions *)
	fun genAutomaton () = let
	    val (stackCounter, callCounters, callStates) = call ()
	    val (retCounters, retStates) = return ()
	    fun finish str = S.find (str, stackCounter)
	    in
	       {call = {cS0=S.init callCounters, 
			cStep=S.mkStep callStates, finish=finish},
		ret  = {rS0=S.init retCounters, rStep=S.mkStep retStates}}
	    end

      end (* SVIDConventions *)

    (* converts locations in staged allocation to argument locations in C *)
    fun argLoc _ (w, S.REG (_, r), K_GPR) = C_GPR (w, r)
      | argLoc _ (w, S.REG (_, r), K_FPR) = C_FPR (w, r)
      | argLoc argOffset (w, S.BLOCK_OFFSET offB, K_GPR) = C_STK (w, T.I.fromInt (wordTy, offB+argOffset))
      | argLoc argOffset (w, S.BLOCK_OFFSET offB, K_FPR) = C_STK (w, T.I.fromInt (wordTy, offB+argOffset))
      | argLoc argOffset (w, S.NARROW (loc, w', k), _) = argLoc argOffset (w', loc, k)
      | argLoc _ (w, S.COMBINE _, _) = raise Fail "impossible"

(* FIXME! *)
    fun containsUnalignedFields cTy = false

    fun szOfCTy cTy = #sz (CSizes.sizeOfTy cTy)
    fun sum ls = List.foldl (op +) 0 ls

    fun firstEightByte ([], eightByte) = (List.rev eightByte, [])
      | firstEightByte (cTy :: cTys, eightByte) = let
        val sz = szOfCTy cTy + sum(List.map szOfCTy eightByte)
	in
	   if (sz <= 8)
	      then firstEightByte (cTys, cTy :: eightByte)
	   else if (sz > 8 andalso sz <= 16)
	      then (List.rev eightByte, cTy :: cTys)
           else raise Fail "invalid unaligned C type"
        end

    (* break the aggregate into at most two eight-byte chunks *)
    fun eightBytes cTys = let
	val (eightByte1, cTys) = firstEightByte(cTys, [])
        in
	    case cTys
	     of [] => [eightByte1]
	      | cTys => [eightByte1, #1(firstEightByte(cTys, []))]
        end

    (* eliminate unions and structs *)
    fun flattenCTy cTy = (case cTy
        of (CTy.C_STRUCT cTys |
	    CTy.C_UNION cTys ) => List.concat (List.map flattenCTy cTys)
	 | cTy => [cTy])

    fun combineKinds (k1, k2) = if (k1 = k2)
	then k1
	else (case (k1, k2)
	       of (K_MEM, _) => K_MEM
		| (_, K_MEM) => K_MEM
		| (K_GPR, _) => K_GPR
		| (_, K_GPR) => K_GPR
		| _ => K_FPR
 	      (* end case*))

    (* classify a non-aggregate C type into its location kind*)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = K_FPR
      | kindOfCTy (cTy as (CTy.C_STRUCT _ | CTy.C_UNION _ | CTy.C_ARRAY _)) = raise Fail ""
      | kindOfCTy _ = K_GPR

    (* classify a C type into its location kinds *)
    fun kindsOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = [K_FPR]
      | kindsOfCTy (cTy as (CTy.C_STRUCT _ | CTy.C_UNION _ | CTy.C_ARRAY _)) = kindsOfAggregate (cTy)
      | kindsOfCTy _ = [K_GPR]

    (* classify each field of the eight byte according to the ABI. at this point,
     * the eightbyte is flattened.
     *)
    and kindsOfEightByte [] = K_MEM
      | kindsOfEightByte [cTy] = kindOfCTy cTy
      | kindsOfEightByte (cTy1 :: cTy2 :: cTys) = let
	val k1 = combineKinds (kindOfCTy cTy1, kindOfCTy cTy2)
	val k2 = kindsOfEightByte(cTy2 :: cTys)
        in
	    combineKinds(k1, k2)
	end

    (* classify an aggregate type according to the ABI *)
    and kindsOfAggregate (cTy as (CTy.C_STRUCT cTys | CTy.C_UNION cTys)) = let
	val {sz, align} = CSizes.sizeOfTy cTy
	val flatCTy = flattenCTy cTy
        in
	    if (sz > 2*8 orelse containsUnalignedFields cTy)
	       then [K_MEM]
               else let
		  val eightBytes = eightBytes cTys
		  in
		       List.map kindsOfEightByte eightBytes
		  end
        end
      | kindsOfAggregate (CTy.C_ARRAY (ty, i)) = raise Fail "todo"
      | kindsOfAggregate _ = raise Fail "impossible"

    (* convert a non-aggregate C type to a location for staged allocation *)
    fun cTyToLoc cty = let
	val {sz, align} = CSizes.sizeOfTy cty
        in
           (sz * 8, kindOfCTy cty, align)
        end

    (* convert a C type to a location for staged allocation *)
(*    fun cTyToLocs cTy = (case cTy
        of CTy.C_STRUCT cTys => let
           val {sz, align} = CSizes.sizeOfTy cTy
           in
	       
	 	  then [cTyToLoc cTy]
	       else let
		   val flatCTy = flattenCTy cTy
		   val eightBytes = eightBytes flatCTy
		   in
		       List.concat (List.map eightByteToLoc eightBytes)
		   end
	   end
	 | CTy.C_UNION cTys => raise Fail "todo"
	 | CTy.C_ARRAY (cTy, i) => raise Fail "todo"
	 | cTy => cTyToLoc cTy
       (* end case *))
	*)    
    structure Test = struct
      val ty1 = CTy.C_STRUCT [CTy.C_STRUCT [CTy.C_unsigned CTy.I_char, CTy.C_unsigned CTy.I_int]]
      val ty2 = CTy.C_STRUCT [CTy.C_signed CTy.I_short]
      val ty3 = CTy.C_STRUCT [CTy.C_signed CTy.I_short, CTy.C_PTR]
      val ty4 = CTy.C_STRUCT [CTy.C_PTR, CTy.C_PTR]
      val ty4 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_unsigned CTy.I_int], CTy.C_PTR]
      val ty5 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_float]]
      val ty6 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float]]
      val ty7 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_STRUCT[CTy.C_float,CTy.C_float],CTy.C_float,CTy.C_float]]
      val ty8 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_STRUCT[CTy.C_float,CTy.C_unsigned CTy.I_int],CTy.C_float,CTy.C_float]]

      fun ebTest () = let	
	fun test (ty, len) =
	    if List.length (eightBytes (flattenCTy ty)) <> len
	       then raise Fail "failed test"
	       else ()
        in
	    List.app test [(ty1, 1), (ty2, 1), (ty3, 2), (ty4, 2)]
        end

      fun kindOf () = let
	  fun test (eb, k) = (kindsOfEightByte eb = k) orelse raise Fail "failed test"
	  fun eb1 ty = hd (eightBytes (flattenCTy ty))
	  fun eb2 ty = hd(tl (eightBytes (flattenCTy ty)))
          in
	      List.all test [(eb1 ty1, K_GPR), (eb1 ty2, K_GPR), (eb2 ty3, K_GPR),
			     (eb1 ty5, K_FPR), (eb1 ty6, K_FPR), (eb2 ty6, K_FPR),
			     (eb1 ty7, K_FPR), (eb2 ty7, K_FPR),
			     (eb1 ty8, K_GPR), (eb2 ty8, K_FPR)]
	  end
    end

    fun layout {conv, retTy, paramTys} = let
	val {call={cS0, cStep, finish}, ret={rS0, rStep}} = SVIDConventions.genAutomaton ()
	(* set up the return value of the call *)
	fun setupReturn () = argLoc 0 (#2 (rStep (rS0, cTyToLoc retTy)))
	val (resLoc, structRetLoc, argOffset) = (case retTy
	     of CTy.C_void => (NONE, NONE, 0)
	      | CTy.C_UNION tys => raise Fail "todo"
	      | CTy.C_STRUCT tys => let
		val {sz, align} = CSizes.sizeOfStruct tys
		in
		  (SOME (setupReturn ()), SOME {szb=sz, align=align}, 8)
		end
	      | _ => (SOME (setupReturn ()), NONE, 0)
	     (* end case *))
        (* set up the arguments for the call *)
	fun setupArgs (str, [], locs) = (finish(str), List.rev locs)
          | setupArgs (str, pTy :: pTys, locs) = let
	    val (str', cLoc) = cStep (str, cTyToLoc pTy)
	    in
		setupArgs (str', pTys, argLoc argOffset cLoc :: locs)
	    end
	val (frameSz, argLocs) = setupArgs (cS0, paramTys, [])
	val argMem = {szb=CSizes.alignAddr (frameSz, frameAlign), align=frameAlign}
        in
	  {argLocs=argLocs, argMem=argMem, resLoc=resLoc, structRetLoc=structRetLoc}
        end (* layout *)

    fun genCall {name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args} = let
	val {argLocs, argMem, resLoc, structRetLoc} = layout(proto)
	val argAlloc = if ((#szb argMem = 0) orelse paramAlloc argMem)
			then []
			else [T.MV (wordTy, C.rsp, T.SUB (wordTy, spReg, 
			      T.LI (T.I.fromInt (wordTy, #szb argMem))))]
	val (copyArgs, gprUses, fprUses) = CCall.copyArgs(args, argLocs)
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
	val (resultRegs, copyResult) = CCall.returnVals(resLoc)
	val callSeq = argAlloc @ copyArgs @ [callStm] @ copyResult
        in
          {callseq=callSeq, result=resultRegs}
        end (* genCall *)

  end (* AMD64SVID *)
