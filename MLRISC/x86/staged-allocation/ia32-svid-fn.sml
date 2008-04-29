(* ia32-svid-fn.sml
 *
 * C calling conventions using staged allocation.
 *)

functor IA32SVIDFn (
    structure T : MLTREE
    val abi : string
    val ix : (T.stm,T.rexp,T.fexp,T.ccexp) X86InstrExt.sext -> T.sext
  (* Note that the fast_loating_point flag must match the one passed
   * to the code generator module.
   *)
    val fast_floating_point : bool ref
  ) (*: C_CALL*) = 
  struct

    structure T = T
    structure C = X86Cells
    structure CB = CellsBasis
    structure CTy = CTypes
    structure IX = X86InstrExt

    val wordTy = 32
    fun gpr r = T.GPR(T.REG(32, r))
    fun fpr f = T.FPR(T.FREG(80, f))

  (* kinds of locations for staged allocation *)
    datatype location_kind
      = K_GPR                   (* pass in general purpose registers *)
      | K_FPR                   (* pass in floating-point registers *)
      | K_MEM                   (* pass in memory *)

  (* staged allocation *)
    structure SA = StagedAllocationFn (
                    structure T = T
		    structure TargetLang =
		      struct
		        datatype location_kind = datatype location_kind
		      end
		    val memSize = 8)

  (* staged-allocation specification for the calling conventions *)
    structure CCs =
      struct

      (* register conventions *)
        val callerSaveRegs = List.map gpr [C.eax, C.ecx, C.edx]
	val calleeSaveRegs = List.map gpr [C.ebx, C.esi, C.edi]
	val spReg = T.REG (32, C.esp)
      (* the C calling convention requires that the FP stack be empty on function
       * entry.  We add the fpStk list to the defs when the fast_floating_point flag
       * is set.
       *)
	val st0 = C.ST 0
	val calleeSaveFRegs = []
      (* the C calling convention requires that the FP stack be empty on function
       * entry.  We add the fpStk list to the defs when the fast_floating_point flag
       * is set.
       *)
	val fpStk = List.tabulate(8, fn i => fpr (C.ST i))


      (* conventions for calling a C function *)
        val alignB = 8
	val cStack = SA.freshCounter()
	val callStages = if (abi = "Mac OS X")
            then [
	      SA.ALIGN_TO (fn ty => Int.max(ty, 16*32)),           (* align the stack to multiples of 16 bytes *)
	      SA.SEQ[
	        SA.WIDEN (fn ty => Int.max(32, ty)),
		SA.OVERFLOW {counter=cStack, blockDirection=SA.UP, maxAlign=alignB}
	    ]
          ] else (* abi <> "Mac OS X" *) [
	      SA.SEQ[
	        SA.WIDEN (fn ty => Int.max(32, ty)),
	        SA.OVERFLOW {counter=cStack, blockDirection=SA.UP, maxAlign=alignB}
	    ]
	  ]

      (* conventions for returning from a C call *)
	val (cInt, retInGpr) = SA.useRegs [(32, C.eax), (32, C.edx)]
	val (cFloat, retInFpr) = SA.useRegs [(80, st0)]
	val returnStages = [
	      SA.CHOICE [
	        (* return in general-purpose register *)
	        (fn (ty, k, str) => k = K_GPR,
		 SA.SEQ [SA.WIDEN (fn ty => Int.max(ty, 32)),
			 retInGpr]),
		(* return in floating-point register *)
		(fn (ty, k, str) => k = K_FPR,
		 SA.SEQ [SA.WIDEN (fn ty => 80), retInFpr])
	      ]
	    ]
	    
	(* initial store *)
	val str0 = SA.init [cInt, cFloat, cStack]

      end (* CallingConventions *)

    structure CCall = CCallFn(
             structure T = T
	     structure C = C
	     val wordTy = wordTy
	     fun offSp 0 = CCs.spReg
	       | offSp offset = T.ADD (32, CCs.spReg, T.LI offset))
		       
    datatype c_arg = datatype CCall.c_arg

    (* classify a C type into its location kind (assuming that aggregates cannot be passed in registers) *)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = K_FPR
      | kindOfCTy (cTy as (CTy.C_STRUCT _ | CTy.C_UNION _ | CTy.C_ARRAY _)) = raise Fail "impossible"
      | kindOfCTy _ = K_GPR

    (* convert a C type to locations for staged allocation *)
    fun cTyToSlots cTy = let
	val {sz, align} = IA32CSizes.sizeOfTy cTy
	(* flatten the C type and compute the locations *)
	val locs = List.map (fn cTy => (sz * 8, kindOfCTy cTy, align))
			    (CTy.flattenCTy cTy)
        in
	  case (cTy, abi)
           of (CTy.C_STRUCT _, "Mac OS X") => 
	      (* on Mac OS X, structs <= 8 bytes are returned in GPRs *)
	      if (sz <= 4)
                 then [(8, K_GPR, align)]
              else if (sz <= 8)
                 then [(8, K_GPR, align), (8, K_GPR, align)]
              else locs
	    | ( (CTy.C_unsigned CTy.I_long_long |
		 CTy.C_signed CTy.I_long_long   ),
		_ ) => 
	      (* 64-bit integers are returned in GPRs *)
	      [(8, K_GPR, align), (8, K_GPR, align)]
	    | _ => locs
        end

  (* convert a finalized staged-allocation location into a C location *)
    fun saToCLoc (ty, SA.REG(_, r), K_GPR) = CCall.C_GPR(ty, r)
      | saToCLoc (ty, SA.REG(_, r), K_FPR) = CCall.C_FPR(ty, r)
      | saToCLoc (ty, SA.BLOCK_OFFSET offB, _) = CCall.C_STK(ty, T.I.fromInt (32, offB))
      | saToCLoc (ty, SA.NARROW(loc, ty', k), _) = saToCLoc (ty, loc, k) 

    fun layout {conv, retTy, paramTys} = let

	(* compute locations for return results *)
	val (resLocs, argOffset, str) = (case retTy
            of CTy.C_void => ([], 0, CCs.str0)
	     | retTy => let
	       val {sz, align} = IA32CSizes.sizeOfTy retTy
  	       (* compute the locations for return values using staged allocation *)
	       val returnStepper = SA.mkStep CCs.returnStages
	       (* finalize locations for the return type *)
	       val (str, locs) = SA.doStagedAllocation(CCs.str0, returnStepper, cTyToSlots retTy)
	       val nBytesAllocated = SA.find(str, CCs.cStack)
	       val argOffset = if (nBytesAllocated > 8) then 4 else 0
	       in
		   (List.map saToCLoc locs, argOffset, str)
               end
            (* end case *))

        (* compute locations for passing arguments *)
	val paramStepper = SA.mkStep CCs.callStages
	fun doParam (paramTy, (str, paramLocss)) = let
	    val (str', paramLocs) = SA.doStagedAllocation(str, paramStepper, cTyToSlots paramTy)
	    in
	       (str', (List.map saToCLoc paramLocs) :: paramLocss)
            end
	val (str, paramLocss) = List.foldl doParam (str, []) paramTys
	val paramLocs = List.rev paramLocss

	in
	   {argLocs=paramLocs,
	    argMem={szb=argOffset, align=argOffset},
	    resLocs=resLocs}
        end (* layout *)

  (* List of registers defined by a C Call with the given return type; this list
   * is the result registers plus the caller-save registers.
   *)
    fun definedRegs resTy = if !fast_floating_point
	  then let
	    val defs = CCs.callerSaveRegs @ CCs.fpStk
	    in
	      case resTy
	       of (CTy.C_unsigned(CTy.I_long_long)) => gpr C.edx :: defs
		| (CTy.C_signed(CTy.I_long_long)) => gpr C.edx :: defs
		| _ => defs
	      (* end case *)
	    end
	  else (case resTy
	     of (CTy.C_float) => fpr CCs.st0 :: CCs.callerSaveRegs
	      | (CTy.C_double) => fpr CCs.st0 :: CCs.callerSaveRegs
	      | (CTy.C_long_double) => fpr (CCs.st0) :: CCs.callerSaveRegs
	      | (CTy.C_unsigned(CTy.I_long_long)) => gpr C.edx :: CCs.callerSaveRegs
	      | (CTy.C_signed(CTy.I_long_long)) => gpr C.edx :: CCs.callerSaveRegs
	      | _ => CCs.callerSaveRegs
	    (* end case *))

    fun fstp (32, f) = T.EXT(ix(IX.FSTPS(f)))
      | fstp (64, f) = T.EXT(ix(IX.FSTPL(f)))
      | fstp (80, f) = T.EXT(ix(IX.FSTPT(f)))
      | fstp (sz, f) = raise Fail ("fstp(" ^ Int.toString sz ^ ",_)")

  (* This annotation is used to indicate that a call returns a fp value 
   * in %st(0) 
   *)
    val fpReturnValueInST0 = #create MLRiscAnnotations.RETURN_ARG C.ST0

    fun genCall {
	    name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args
	  } = let
	  val {argLocs, argMem, resLocs} = layout proto
	(* instruction to allocate space for arguments *)
	  val argAlloc = if ((#szb argMem = 0) orelse paramAlloc argMem)
		then []
		else [T.MV(wordTy, C.esp, T.SUB(wordTy, CCs.spReg, T.LI(IntInf.fromInt(#szb argMem))))]
	  val (copyArgs, gprUses, fprUses) = CCall.copyArgs(args, argLocs)

	(* the SVID specifies that the caller pops arguments, but the callee
	 * pops the arguments in a stdcall on Windows.  I'm not sure what other
	 * differences there might be between the SVID and Windows ABIs. (JHR)
	 *)
	  val calleePops = (case #conv proto
		 of (""|"ccall") => false
		  | "stdcall" => true
		  | conv => raise Fail (concat [
			"unknown calling convention \"", String.toString conv, "\""
		      ])
		(* end case *))

	  val explicitArgSzB = 0

	(* code to pop the arguments from the stack *)
	  val popArgs = if calleePops orelse (explicitArgSzB = 0)
		then []
		else [T.MV(wordTy, C.esp, T.ADD(wordTy, CCs.spReg, T.LI(IntInf.fromInt explicitArgSzB)))]

	(* code to copy the result into fresh pseudo registers *)
	  val (resultRegs, copyResult) = (case resLocs
		 of [] => ([], [])
		  | [CCall.C_GPR(ty, r)] => let
		      val resReg = C.newReg()
		      in
			([T.GPR(T.REG(ty, resReg))], [T.COPY(ty, [resReg], [r])])
		      end
		  | [CCall.C_FPR(ty, r)] => let
		      val resReg = C.newFreg()
		      val res = [T.FPR(T.FREG(ty, resReg))]
		      in
        	      (* If we are using fast floating point mode then do NOT 
        	       * generate FSTP.
        	       * --- Allen 
        	       *)
			if !fast_floating_point
			  then (res, [T.FCOPY(ty, [resReg], [r])])
			  else (res, [fstp(ty, T.FREG(ty, resReg))])
		      end
		  | _ => raise Fail "bogus result location"
		(* end case *))

	  val defs = definedRegs(#retTy proto)
	  val { save, restore } = saveRestoreDedicated defs

	  val callStm = T.CALL{
		  funct=name, targets=[], defs=defs, uses=[], 
		  region = T.Region.memory,
		  pops = if calleePops
		      then Int32.fromInt(#szb argMem)
		      else Int32.fromInt(#szb argMem - explicitArgSzB)
		}
	  val callStm = (case callComment
		 of NONE => callStm
		  | SOME c => T.ANNOTATION (callStm, #create MLRiscAnnotations.COMMENT c)
		(* end case *))

	  val callStm = if !fast_floating_point
		andalso ((#retTy proto = CTy.C_float)
		  orelse (#retTy proto = CTy.C_double)
		  orelse (#retTy proto = CTy.C_long_double))
		then T.ANNOTATION(callStm, fpReturnValueInST0)
		else callStm

	(* assemble the call sequence *)
	  val callSeq = argAlloc @ copyArgs @ save @ [callStm] @ restore @ popArgs @ copyResult

          in
   	    {callseq=callSeq, result=resultRegs}
          end

  end (* IA32SVIDFn *)
