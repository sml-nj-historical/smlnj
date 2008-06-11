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
    val spReg = T.REG (32, C.esp)

    (* the C calling convention requires that the FP stack be empty on function
     * entry.  We add the fpStk list to the defs when the fast_floating_point flag
     * is set.
     *)
    val st0 = C.ST 0

    datatype location_kind = datatype CLocationKinds.location_kind

    structure CCall = CCallFn(
             structure T = T
	     structure C = C
	     val wordTy = wordTy
	     fun offSp 0 = spReg
	       | offSp offset = T.ADD (32, spReg, T.LI offset))
		
    datatype c_arg = datatype CCall.c_arg
    datatype arg_location = datatype CCall.arg_location

    structure SA = StagedAllocationFn (
                    type reg = T.reg
		    datatype location_kind = datatype location_kind
		    val memSize = 8)

    structure CCs = IA32CConventionsFn (
		      type reg = T.reg
		      val eax = C.eax
		      val edx = C.edx
		      val st0 = st0
		      structure SA = SA
		    )

    val calleeSaveRegs = [C.ebx, C.esi, C.edi]
    val callerSaveRegs = [C.eax, C.ecx, C.edx]
    val calleeSaveFRegs = []
    val callerSaveFRegs = []

    (* classify a C type into its location kind (assuming that aggregates cannot be passed in registers) *)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = K_FPR
      | kindOfCTy (CTy.C_unsigned _ | CTy.C_signed _ | CTy.C_PTR | CTy.C_ARRAY _) = K_GPR

    (* convert a C type to slots for staged allocation *)
    fun cTyToSlots cTy = let
	val {sz, align} = IA32CSizes.sizeOfTy cTy
	(* compute argument slots for the flattened C type *)
	val slots = List.map (fn cTy => (sz * 8, kindOfCTy cTy, align))
			    (CTy.flattenCTy cTy)
        in
	  case (cTy, abi)
           of (CTy.C_STRUCT _, "Darwin") => 
	      (* for Darwin, structs <= 8 bytes are returned in GPRs *)
	      if (sz <= 4)
                 then [(8, K_GPR, align)]
              else if (sz <= 8)
                 then [(8, K_GPR, align), (8, K_GPR, align)]
              else slots
	    | ( (CTy.C_unsigned CTy.I_long_long |
		 CTy.C_signed CTy.I_long_long   ),
		_ ) => 
	      (* 64-bit integers are returned in GPRs *)
	      [(8, K_GPR, align), (8, K_GPR, align)]
	    | _ => slots
        end

  (* convert a finalized staged-allocation location into a C location *)
    fun saToCLoc (ty, SA.REG(_, r), K_GPR) = CCall.C_GPR(ty, r)
      | saToCLoc (ty, SA.REG(_, r), K_FPR) = CCall.C_FPR(ty, r)
      | saToCLoc (ty, SA.BLOCK_OFFSET offB, K_GPR) = CCall.C_STK(ty, T.I.fromInt (32, offB))
      | saToCLoc (ty, SA.BLOCK_OFFSET offB, K_FPR) = CCall.C_FSTK(ty, T.I.fromInt (32, offB))
      | saToCLoc (ty, SA.NARROW(loc, ty', _), k) = saToCLoc (ty, loc, k) 


  (* returns the stack location for storing a given parameter slot *)
    fun layoutParam paramStepper (paramSlot, (str, paramLocss)) = let
	    val (str', paramLocs) = SA.doStagedAllocation(str, paramStepper, paramSlot)
	    in
	       (str', paramLocs :: paramLocss)
            end
  
  (* compute the stack locations for passing parameters *)
    fun layoutParams (str, paramTys) = let
	    val paramStepper = SA.mkStep CCs.callStages
	    val paramSlots = List.map cTyToSlots paramTys
	    val (str, paramLocs) = List.foldl (layoutParam paramStepper) (str, []) paramSlots
	    val paramCLocs = List.map (List.map saToCLoc) paramLocs
	    in
	       (str, List.rev paramCLocs)
            end

  (* compute the parameter passing and return for a given C call *)
    fun layout {conv, retTy, paramTys} = let
	(* compute locations for return results *)
	val (resLocs, structRetLoc, str) = (case retTy
            of CTy.C_void => ([], NONE, CCs.str0)
	     | retTy => let
	       val {sz, align} = IA32CSizes.sizeOfTy retTy
  	       (* compute the locations for return values using staged allocation *)
	       val returnStepper = SA.mkStep CCs.returnStages
	       (* finalize locations for the return type *)
	       val (str, locs) = SA.doStagedAllocation(CCs.str0, returnStepper, cTyToSlots retTy)
	       val nBytesAllocated = SA.find(str, CCs.cStack)
	       in
		   (List.map saToCLoc locs, SOME {szb=sz, align=align}, str)
               end
            (* end case *))

	val (str, paramLocs) = layoutParams (str, paramTys)

	(* number of bytes allocated for the call *)
	val cStkSzB = let
             val n = SA.find(str, CCs.cStack)
             in
                if (abi = "Mac OS X")
		   then IA32CSizes.alignAddr(n, 16)
                   else n
             end
	in
	   {argLocs=paramLocs, argMem={szb=cStkSzB, align=4}, structRetLoc=structRetLoc, resLocs=resLocs}
        end (* layout *)

    val callerSaveRegs' = List.map gpr calleeSaveRegs
    val calleeSaveRegs' = List.map gpr calleeSaveRegs
    val calleeSaveFRegs' = []
    val callerSaveFRegs' = []

    (* the C calling convention requires that the FP stack be empty on function
     * entry.  We add the fpStk list to the defs when the fast_floating_point flag
     * is set.
     *)
    val fpStk = List.tabulate(8, fn i => fpr (C.ST i))


  (* List of registers defined by a C Call with the given return type; this list
   * is the result registers plus the caller-save registers.
   *)
    fun definedRegs resTy = if !fast_floating_point
	  then let
	    val defs = callerSaveRegs' @ fpStk
	    in
	      case resTy
	       of (CTy.C_unsigned(CTy.I_long_long)) => gpr C.edx :: defs
		| (CTy.C_signed(CTy.I_long_long)) => gpr C.edx :: defs
		| _ => defs
	      (* end case *)
	    end
	  else (case resTy
	     of (CTy.C_float) => fpr st0 :: callerSaveRegs'
	      | (CTy.C_double) => fpr st0 :: callerSaveRegs'
	      | (CTy.C_long_double) => fpr st0 :: callerSaveRegs'
	      | (CTy.C_unsigned(CTy.I_long_long)) => gpr C.edx :: callerSaveRegs'
	      | (CTy.C_signed(CTy.I_long_long)) => gpr C.edx :: callerSaveRegs'
	      | _ => callerSaveRegs'
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
	  val {argLocs, argMem, structRetLoc, resLocs} = layout proto

	(* for functions that return a struct/union, pass the location as an
	 * implicit first argument.  Because the callee removes this implicit
	 * argument from the stack, we must also keep track of the size of the
	 * explicit arguments.
	 *)
	  val (args, argLocs, explicitArgSzB) = (case structRetLoc
		 of SOME pos =>
		      (ARG(structRet pos)::args, [CCall.C_STK(wordTy, T.I.fromInt (wordTy, 0))]::argLocs, #szb argMem)
		  | NONE => (args, argLocs, #szb argMem)
		(* end case *))

	(* instruction to allocate space for arguments *)
	  val argAlloc = if ((#szb argMem = 0) orelse paramAlloc argMem)
	        then []
                else if abi = "Darwin"	
		      then let
		       (* align the frame on a 16-byte boundary *)
			val szb = IA32CSizes.alignAddr(#szb argMem + 2*4, 16)-2*4
		        in
			  [T.MV(wordTy, C.esp, T.SUB(wordTy, spReg, T.LI(IntInf.fromInt szb)))]
			end
		else [T.MV(wordTy, C.esp, T.SUB(wordTy, spReg, T.LI(IntInf.fromInt(#szb argMem))))]
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

	(* code to pop the arguments from the stack *)
	  val popArgs = if calleePops orelse (explicitArgSzB = 0)
		then []
		else [T.MV(wordTy, C.esp, T.ADD(wordTy, spReg, T.LI(IntInf.fromInt explicitArgSzB)))]

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
