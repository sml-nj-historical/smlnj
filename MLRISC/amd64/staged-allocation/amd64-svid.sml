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
    fun lit i = T.LI (T.I.fromInt (wordTy, i))
    fun gpr r = T.GPR (T.REG (wordTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))
    fun sum ls = List.foldl (op +) 0 ls

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
				  type reg = T.reg
                                  datatype location_kinds = datatype location_kind
				  val memSize = 8
				 )

    val stack = T.Region.stack

    datatype c_arg =
	     (* rexp specifies integer or pointer; if the 
              * corresponding parameter is a C struct, then 
	      * this argument is the address of the struct. 
	      *)
	     ARG of T.rexp	
	   (* fexp specifies floating-point argument *)		    
	   | FARG of T.fexp

    (* An arg_location specifies the location of arguments/parameters
     * for a C call.  Offsets are given with respect to the low end 
     * of the parameter area. *)
    datatype arg_location =
	C_GPR  of (T.ty * T.reg) (* integer/pointer argument in register *)
      | C_FPR  of (T.fty * T.reg) (* floating-point argument in register *)
      | C_STK  of (T.ty * T.I.machine_int)  (* integer/pointer argument on the call stack *)
      | C_FSTK of (T.fty * T.I.machine_int) (* floating-point argument on the call stack *)
		     
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
	(* parameter passing conventions *)
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
	(* return conventions *)
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

    fun szOfCTy cTy = #sz (CSizes.sizeOfTy cTy)

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

    fun combineKinds (k1, k2) = if (k1 = k2)
	then k1
	else (case (k1, k2)
	       of (K_MEM, _) => K_MEM
		| (_, K_MEM) => K_MEM
		| (K_GPR, _) => K_GPR
		| (_, K_GPR) => K_GPR
		| _ => K_FPR
 	      (* end case*))

    (* classify a C type into its location kind (assuming that aggregates cannot be passed in registers) *)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = K_FPR
      | kindOfCTy (cTy as (CTy.C_STRUCT _ | CTy.C_UNION _ | CTy.C_ARRAY _)) = K_MEM
      | kindOfCTy _ = K_GPR

    (* classify an eightbyte by analyzing the types of its fields *)
    fun kindOfEightByte [] = K_MEM
      | kindOfEightByte [cTy] = kindOfCTy cTy
      | kindOfEightByte (cTy1 :: cTy2 :: cTys) = let
	val k1 = combineKinds (kindOfCTy cTy1, kindOfCTy cTy2)
	val k2 = kindOfEightByte(cTy2 :: cTys)
        in
	    combineKinds(k1, k2)
	end

    fun containsUnalignedFields cTy = (case cTy
        of (CTy.C_STRUCT cTys | CTy.C_UNION cTys) => 
	   List.all (fn cTy => #sz (CSizes.sizeOfTy cTy) mod 8 = 0) cTys
	 | _ => false)

    (* classify a C type into its location kinds (aggregates might be passed in registers) *)
    fun kindsOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = [K_FPR]
      | kindsOfCTy (cTy as CTy.C_STRUCT cTys) = let
	val {sz, align} = CSizes.sizeOfTy cTy
	val flatCTy = CTypes.flattenCTy cTy
        in
	    if (sz > 2*8 orelse containsUnalignedFields cTy)
	       then List.tabulate (sz div 8, fn _ => K_MEM)       (* K_MEM ^ (number of eightbytes of the type) *)
               else let
		  val eightBytes = eightBytes flatCTy
		  in
		       List.map kindOfEightByte eightBytes
		  end
        end
      | kindsOfCTy (cTy as CTy.C_UNION cTys) = raise Fail "todo"
      | kindsOfCTy (CTy.C_ARRAY (ty, len)) = raise Fail "todo"
      | kindsOfCTy _ = [K_GPR]

    (* convert a non-aggregate C type to a location for staged allocation *)
    fun cTyToLoc cty = let
	val {sz, align} = CSizes.sizeOfTy cty
        in
           (sz * 8, kindOfCTy cty, align)
        end

    fun cTyToLocs cTy = let
	val {sz, align} = CSizes.sizeOfTy cTy
	val ks = kindsOfCTy cTy
        in
	    List.map (fn k => (sz * 8, k, align)) ks
        end

    (* converts location information to an argument location in C *)
    fun saInfoToCLoc _ (w, S.REG (_, r), K_GPR) = C_GPR (w, r)
      | saInfoToCLoc _ (w, S.REG (_, r), K_FPR) = C_FPR (w, r)
      | saInfoToCLoc argOffset (w, S.BLOCK_OFFSET offB, (K_GPR | K_FPR | K_MEM)) = 
	C_STK (w, T.I.fromInt (wordTy, offB+argOffset))
      | saInfoToCLoc argOffset (w, S.NARROW (loc, w', k), _) = saInfoToCLoc argOffset (w', loc, k)
      | saInfoToCLoc _ _ = raise Fail "impossible"

    fun layout {conv, retTy, paramTys} = let
	val {call={cS0, cStep, finish}, ret={rS0, rStep}} = SVIDConventions.genAutomaton ()
	(* convert return locations for staged allocation to return locations for C *)
	fun returnSALocsToCLocs () = saInfoToCLoc 0 (#2 (rStep (rS0, cTyToLoc retTy)))
	val (resLoc, structRetLoc, argOffset) = (case retTy
	     of CTy.C_void => (NONE, NONE, 0)
	      | CTy.C_STRUCT tys => let
		val {sz, align} = CSizes.sizeOfStruct tys
		in
		  (SOME (returnSALocsToCLocs ()), SOME {szb=sz, align=align}, 8)
		end
	      | CTy.C_UNION tys => let
		val {sz, align} = CSizes.sizeOfUnion tys
		in
		  (SOME (returnSALocsToCLocs ()), SOME {szb=sz, align=align}, 8)
		end
	      | CTy.C_ARRAY (ty, len) => raise Fail "todo"
	      | _ => (SOME (returnSALocsToCLocs ()), NONE, 0)
	     (* end case *))
	(* convert parameter locations for staged allocation to parameter locations for C *)
	fun paramSALocsToCLocs (str, [], cLocss) = (finish str, List.rev cLocss)
	  | paramSALocsToCLocs (str, sls :: slss, cLocss) = let
 	    fun doSteps ([], str, lis) = (str, List.rev lis)
	      | doSteps (sl :: sls, str, lis) = let
		val (str', li) = cStep(str, sl)
		in
		    doSteps(sls, str', li :: lis)
		end
	    val (str', lis) = doSteps(sls, str, [])
	    val cLocs = List.map (saInfoToCLoc argOffset) lis
	    in
		paramSALocsToCLocs (str', slss, cLocs :: cLocss)
	    end
	val (frameSz, argLocs) = paramSALocsToCLocs (cS0, List.map cTyToLocs paramTys, [])
	val argMem = {szb=CSizes.alignAddr (frameSz, frameAlign), align=frameAlign}
        in
	  {argLocs=argLocs, argMem=argMem, resLoc=resLoc, structRetLoc=structRetLoc}
        end (* layout *)

    fun offSp 0 = spReg
      | offSp offset = T.ADD (wordTy, spReg, T.LI offset)

    fun copyToReg (mty, r, e) = let
	val tmp = C.newReg ()
        in
	    [T.COPY (mty, [r], [tmp]), T.MV (mty, tmp, e)]
        end

    fun copyToFReg (mty, r, e) = let
	val tmp = C.newFreg ()
        in
	    [T.FCOPY (mty, [r], [tmp]), T.FMV (mty, tmp, e)] 
(*	    [T.FMV (mty, r, T.FREG(mty, tmp)), T.FMV (mty, tmp, e)] *)
(*	    [T.FMV (mty, r, e)]*)
        end

    (* generate MLRISC statements for copying a C argument to a parameter / return location *)
    fun copyLoc arg (i, loc, (stms, gprs, fprs)) = (case (arg, loc)
          (* GPR arguments *)
         of (ARG (e as T.REG _), C_STK (mty, offset)) =>
	    (T.STORE (wordTy, offSp offset, e, stack) :: stms, gprs, fprs)
	  | (ARG (T.LOAD (ty, e, rgn)), C_GPR (mty, r)) =>
	    (copyToReg(mty, r, T.LOAD (ty, T.ADD(wordTy, e, lit (i*8)), rgn)) @ stms, r :: gprs, fprs)
	  | (ARG (T.LOAD (ty, e, rgn)), C_STK (mty, offset)) => let
	    val tmp = C.newReg ()
	    in
		(T.STORE (ty, offSp offset, T.REG (ty, tmp), stack) :: 
		 T.MV (ty, tmp, T.LOAD (ty, T.ADD(wordTy, e, lit (i*8)), rgn)) :: stms, gprs, fprs)
	    end
	  | (ARG e, C_STK (mty, offset)) => let
	     val tmp = C.newReg ()
	     in
		(T.STORE (wordTy, offSp offset, T.REG (wordTy, tmp), stack) ::T.MV (wordTy, tmp, e) :: stms, gprs, fprs)
	      end
	  | (ARG e, C_GPR (mty, r)) => (copyToReg(mty, r, e) @ stms, r :: gprs, fprs)
          (* floating-point arguments *)
	  | (FARG (e as T.FREG _), C_STK (mty, offset)) =>
	    (T.FSTORE (mty, offSp offset, e, stack) :: stms, gprs, fprs)
	  | (ARG (T.LOAD (ty, e, rgn)), C_FPR (mty, r)) =>
	    (copyToFReg(mty, r, T.FLOAD (ty, T.ADD(wordTy, e, lit (i*8)), rgn)) @ stms, gprs, (mty, r) :: fprs)
	  | (FARG (T.FLOAD (ty, e, rgn)), C_STK (mty, offset)) => let
	    val tmp = C.newFreg ()
	    in
		(T.FSTORE (wordTy, offSp offset, T.FREG (wordTy, tmp), stack) :: 
		 T.FMV (wordTy, tmp, T.FLOAD (ty, T.ADD(wordTy, e, lit (i*8)), rgn)) :: stms, gprs, fprs)
	    end
	  | (FARG e, C_STK (mty, offset)) => let
	    val tmp = C.newFreg ()
	    in
		(T.FSTORE (wordTy, offSp offset, T.FREG (wordTy, tmp), stack) :: T.FMV (wordTy, tmp, e) :: stms, gprs, fprs)
	    end
	  | (FARG e, C_FPR (mty, r)) => (copyToFReg(mty, r, e) @ stms, gprs, (mty, r) :: fprs)
	  | _ => raise Fail "invalid arg / location combination"
         (* end case *))

    fun copyArgLocs (arg, locs, (stms, gprs, fprs)) = 
	ListPair.foldl (copyLoc arg) (stms, gprs, fprs) (List.tabulate(List.length locs, fn i => i), locs)

    (* copy C arguments into parameter locations *)
    fun copyArgs (args, argLocs) = let
	val (stms, gprs, fprs) = ListPair.foldl copyArgLocs ([], [], []) (args, argLocs)
        in
	    (List.rev stms, gprs, fprs)
        end

    (* copy the return value into the result location *)
    fun returnVals resLoc = (case resLoc
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

    fun genCall {name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args} = let
	val {argLocs, argMem, resLoc, structRetLoc} = layout(proto)
	val argAlloc = if ((#szb argMem = 0) orelse paramAlloc argMem)
			then []
			else [T.MV (wordTy, C.rsp, T.SUB (wordTy, spReg, 
			      T.LI (T.I.fromInt (wordTy, #szb argMem))))]
	val (copyArgs, gprUses, fprUses) = copyArgs(args, argLocs)
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
	val (resultRegs, copyResult) = returnVals(resLoc)
	val callSeq = argAlloc @ copyArgs @ [callStm] @ copyResult
        in
          {callseq=callSeq, result=resultRegs}
        end (* genCall *)

    (* unit testing code *)
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
      val ty9 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float]]
      val ty10 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_float,CTy.C_float, CTy.C_STRUCT[CTy.C_float,CTy.C_unsigned CTy.I_int]]]
      val ty11 = CTy.C_STRUCT [CTy.C_PTR, CTy.C_float, CTy.C_float, CTy.C_float]

      fun ebTest () = let	
	fun test (ty, len) =
	    if List.length (eightBytes (CTypes.flattenCTy ty)) <> len
	       then raise Fail "failed test"
	       else ()
        in
	    List.app test [(ty1, 1), (ty2, 1), (ty3, 2), (ty4, 2)]
        end

      fun kindOfEB () = let
	  fun test (eb, k) = (kindOfEightByte eb = k) orelse raise Fail "failed test"
	  fun eb1 ty = hd (eightBytes (CTypes.flattenCTy ty))
	  fun eb2 ty = hd(tl (eightBytes (CTypes.flattenCTy ty)))
          in
	      List.all test [(eb1 ty1, K_GPR), (eb1 ty2, K_GPR), (eb2 ty3, K_GPR),
			     (eb1 ty5, K_FPR), (eb1 ty6, K_FPR), (eb2 ty6, K_FPR),
			     (eb1 ty7, K_FPR), (eb2 ty7, K_FPR),
			     (eb1 ty8, K_GPR), (eb2 ty8, K_FPR)]
	  end

      fun li2k (_, k, _) = k

      fun cTyToLocsT () = let
	  fun test (lis : S.slot list, ks2 : location_kind list) = let
	      val ks1 = List.map li2k lis
              in
	         (List.length ks1 = List.length ks2) andalso (ListPair.all (op =) (ks1, ks2))
	      end
	  val tests = [
(*	               (ty2, [K_GPR]), (ty1, [K_GPR]), (ty3, [K_GPR, K_GPR]), (ty4, [K_GPR, K_GPR]), 
		       (ty5, [K_FPR]), (ty6, [K_FPR, K_FPR]),
		       (ty7, [K_FPR, K_FPR]), (ty8, [K_GPR, K_FPR]),
		       (ty9, [K_MEM]), (ty10, [K_FPR, K_GPR]),
*)
		       (ty11, [K_GPR, K_FPR, K_FPR, K_FPR])
				       ]
	  val (ts, anss) = ListPair.unzip tests
          in
	     ListPair.all test (List.map cTyToLocs ts, anss) orelse raise Fail "failed test"
          end

      val proto1 = {conv="ccall", retTy=CTy.C_void, paramTys=[ty11]}
      fun paramTyEx () = layout proto1
      fun argEx () = genCall {name=T.LOAD(32, lit 1024, mem), proto=proto1, paramAlloc=fn _ => false, 
			      structRet=fn _ => raise Fail "", 
			      saveRestoreDedicated=fn _ => raise Fail "", 
			      callComment=NONE, args=[ARG (T.LOAD(32, lit 0, mem))]}

    end

  end (* AMD64SVID *)
