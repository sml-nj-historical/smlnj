(* ia32-svid-fn.sml
 *
 *
 * C function calls for the IA32 using the System V ABI
 *
 * Register conventions:
 *
 *    %eax	return value		(caller save)
 *    %ebx	global offset for PIC	(callee save)
 *    %ecx	scratch			(caller save)
 *    %edx	extra return/scratch	(caller save)
 *    %ebp	optional frame pointer	(callee save)
 *    %esp	stack pointer		(callee save)
 *    %esi	locals			(callee save)
 *    %edi	locals			(callee save)
 *
 *    %st(0)	top of FP stack; FP return value
 *    %st(1..7)	FP stack; must be empty on entry and return
 *
 * Calling convention:
 *
 *    Return result:
 *	+ Integer and pointer results are returned in %eax.  Small
 *	  integer results are not promoted.
 *	+ 64-bit integers (long long) returned in %eax/%edx
 *	+ Floating point results are returned in %st(0) (all types).
 *	+ Struct results are returned in space provided by the caller.
 *	  The address of this space is passed to the callee as an
 *	  implicit 0th argument, and on return %eax contains this
 *	  address.  The called function is responsible for removing
 *	  this argument from the stack using a "ret $4" instruction.
 *	  NOTE: the MacOS X ABI returns small structs in %eax/%edx.
 *
 *    Function arguments:
 *	+ Arguments are pushed on the stack right to left.
 *	+ Integral and pointer arguments take one word on the stack.
 *	+ float arguments take one word on the stack.
 *	+ double arguments take two words on the stack.  The i386 ABI does
 *	  not require double word alignment for these arguments.
 *	+ long double arguments take three words on the stack.
 *	+ struct arguments are padded out to word length.
 *
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
    val spReg = T.REG (32, C.esp)

    structure CCall = CCallFn(
             structure T = T
	     structure C = C
	     val wordTy = wordTy
	     fun offSp 0 = spReg
	       | offSp offset = T.ADD (32, spReg, T.LI offset))
		
    datatype c_arg = datatype CCall.c_arg
    datatype arg_location = datatype CCall.arg_location
    datatype location_kinds = datatype CCall.location_kinds

    structure SA = StagedAllocationFn (
                    type reg = T.reg
		    datatype location_kinds = datatype location_kinds
		    val memSize = 8)

    val calleeSaveRegs = [C.eax, C.ecx, C.edx]
    val callerSaveRegs = [C.ebx, C.esi, C.edi]
    val calleeSaveFRegs = []
    val callerSaveFRegs = []

  (* calling conventions in the Staged Allocation language *)
    structure CCs =
      struct

      (* register conventions *)
        val callerSaveRegs = List.map gpr calleeSaveRegs
	val calleeSaveRegs = List.map gpr calleeSaveRegs
	val calleeSaveFRegs = []
	val callerSaveFRegs = []

      (* the C calling convention requires that the FP stack be empty on function
       * entry.  We add the fpStk list to the defs when the fast_floating_point flag
       * is set.
       *)
	val st0 = C.ST 0
      (* the C calling convention requires that the FP stack be empty on function
       * entry.  We add the fpStk list to the defs when the fast_floating_point flag
       * is set.
       *)
	val fpStk = List.tabulate(8, fn i => fpr (C.ST i))

      (* conventions for calling a C function *)
        val alignB = 4
	val cStack = SA.freshCounter()
	val callStages = [
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

      end (* CCs *)

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
           of (CTy.C_STRUCT _, "Mac OS X") => 
	      (* on Mac OS X, structs <= 8 bytes are returned in GPRs *)
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
      | saToCLoc (ty, SA.BLOCK_OFFSET offB, _) = CCall.C_STK(ty, T.I.fromInt (32, offB))
      | saToCLoc (ty, SA.NARROW(loc, ty', k), _) = saToCLoc (ty, loc, k) 

    val frameAlign = 8

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

        (* compute locations for passing arguments *)
	val paramStepper = SA.mkStep CCs.callStages
	fun doParam (paramTy, (str, paramLocss)) = let
	    val (str', paramLocs) = SA.doStagedAllocation(str, paramStepper, cTyToSlots paramTy)
	    in
	       (str', (List.map saToCLoc paramLocs) :: paramLocss)
            end
	val (str, paramLocss) = List.foldl doParam (str, []) paramTys
	val paramLocs = List.rev paramLocss

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
	  val {argLocs, argMem, structRetLoc, resLocs} = layout proto

	(* for functions that return a struct/union, pass the location as an
	 * implicit first argument.  Because the callee removes this implicit
	 * argument from the stack, we must also keep track of the size of the
	 * explicit arguments.
	 *)
	  val (args, argLocs, explicitArgSzB) = (case structRetLoc
		 of SOME pos =>
		      (ARG(structRet pos)::args, [CCall.C_STK(wordTy, T.I.fromInt (wordTy, 0))]::argLocs, #szb argMem - 4)
		  | NONE => (args, argLocs, #szb argMem)
		(* end case *))

	(* instruction to allocate space for arguments *)
	  val argAlloc = if ((#szb argMem = 0) orelse paramAlloc argMem)
		then []
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
	  val callSeq = argAlloc @ copyArgs @ save @ [callStm] @ restore(* @ popArgs*) @ copyResult

          in
   	    {callseq=callSeq, result=resultRegs}
          end

  end (* IA32SVIDFn *)
