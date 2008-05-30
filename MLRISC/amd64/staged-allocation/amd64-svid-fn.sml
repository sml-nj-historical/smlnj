(* amd64-svid-fn.sml
 *
 * C calling conventions for the AMD64. We use the technique of Staged Allocation (see
 * MLRISC/staged-allocation).
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor AMD64SVIDFn (
    structure T : MLTREE
  ) = struct

    structure T = T
    structure C = AMD64Cells
    structure CB = CellsBasis
    structure CTy = CTypes

    val wordTy = 64
    val mem = T.Region.memory
    val stack = T.Region.stack

    fun lit i = T.LI (T.I.fromInt (wordTy, i))
    fun gpr r = T.GPR (T.REG (wordTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))
    fun sum ls = List.foldl (op +) 0 ls
    fun szBOfCTy cTy = #sz (CSizes.sizeOfTy cTy)
    fun alignBOfCTy cTy = #align (CSizes.sizeOfTy cTy)
    val spReg = T.REG (wordTy, C.rsp)
    fun offSp 0 = spReg
      | offSp offset = T.ADD (wordTy, spReg, T.LI offset)

    structure CCall = CCallFn (
		        structure T = T
			structure C = C
			val wordTy = wordTy
			val offSp = offSp)

    datatype c_arg = datatype CCall.c_arg
    datatype arg_location = datatype CCall.arg_location
    datatype location_kinds = datatype CCall.location_kinds

    structure SA = StagedAllocationFn (
                         type reg = T.reg
                         datatype location_kinds = datatype location_kinds
			 val memSize = 8 (* bytes *))

    structure CCs =
      struct

        fun toGpr r = (wordTy, r)
	fun toGprs gprs = List.map toGpr gprs
	fun toFpr r = (64, r)
	fun toFprs fprs = List.map toFpr fprs 

	val calleeSaveRegs = toGprs [C.rbx, C.r12, C.r13, C.r14, C.r15]
	val callerSaveRegs = toGprs [C.rax, C.rcx, C.rdx, C.rsi, C.rdi, C.r8, C.r9, C.r10, C.r11]
	val callerSaveFRegs = toFprs (C.Regs CB.FP {from=0, to=15, step=1})
	val calleeSaveFRegs = []

	val frameAlignB = 16
	val maxAlign = 16

      (* conventions for returning arguments *)
	val gprRets = toGprs [C.rax, C.rdx]
	val fprRets = toFprs [C.xmm0, C.xmm1]
	val (cRetFpr, ssFloat) = SA.useRegs fprRets
	val (cRetGpr, ssGpr) = SA.useRegs gprRets
	val cCallStk = SA.freshCounter ()
	val returnStages = [
	    SA.CHOICE [
	        (* return in general-purpose register *)
	          (fn (w, k, str) => k = K_GPR,
	           SA.SEQ [SA.WIDEN (fn w => Int.max (wordTy, w)), ssGpr]),
		(* return in floating-point register *)
		  (fn (w, k, str) => k = K_FPR,
	           SA.SEQ [SA.WIDEN (fn w => Int.max (64, w)), ssFloat]),
		(* return in a memory location *)
		  (fn (w, k, str) => k = K_MEM,
		 (* FIXME! *)
		   SA.OVERFLOW {counter=cCallStk, blockDirection=SA.UP, maxAlign=maxAlign}) ]
	     ]

      (* conventions for passing arguments *)
	val gprParams = toGprs [C.rdi, C.rsi, C.rdx, C.rcx, C.r8, C.r9]
	val fprParams = toFprs [C.xmm0, C.xmm1, C.xmm2, C.xmm3, C.xmm4, C.xmm5, C.xmm6, C.xmm7]
	val cCallGpr = SA.freshCounter ()
	val cCallFpr = SA.freshCounter ()
      (* initial store *)

	val str0 = SA.init [cCallStk, cCallGpr, cCallFpr, cRetFpr, cRetGpr]

	val callStages = [ 
	      SA.CHOICE [
	      (* pass in general-purpose register *)
	      (fn (w, k, str) => k = K_GPR, SA.SEQ [
					    SA.WIDEN (fn w => Int.max (wordTy, w)),
					    SA.BITCOUNTER cCallGpr,
					    SA.REGS_BY_BITS (cCallGpr, gprParams) ]),
	      (* pass in floating point register *)
	      (fn (w, k, str) => k = K_FPR, SA.SEQ [
					    SA.WIDEN (fn w => Int.max (64, w)),
					    SA.BITCOUNTER cCallFpr,
					    SA.REGS_BY_BITS (cCallFpr, fprParams) ]),
	      (* pass on the stack *)
	      (fn (w, k, str) => k = K_MEM,
	       SA.OVERFLOW {counter=cCallStk, blockDirection=SA.UP, maxAlign=maxAlign}) 
	      ],
	      SA.OVERFLOW {counter=cCallStk, blockDirection=SA.UP, maxAlign=maxAlign}
	]

      end  (* CCs *)

    val calleeSaveRegs : CB.cell list = List.map (fn (_, r) => r) CCs.calleeSaveRegs
    val calleeSaveFRegs :CB.cell list = List.map (fn (_, r) => r) CCs.calleeSaveFRegs
    val callerSaveRegs : CB.cell list = List.map (fn (_, r) => r) CCs.callerSaveRegs
    val callerSaveFRegs :CB.cell list = List.map (fn (_, r) => r) CCs.callerSaveFRegs

  (* convert a list of C types to a list of eight bytes *)
    fun eightBytesOfCTys ([], [], ebs) = List.rev (List.map List.rev ebs)
      | eightBytesOfCTys ([], eb, ebs) = List.rev (List.map List.rev (eb :: ebs))
      | eightBytesOfCTys (cTy :: cTys, eb, ebs) = let
	    val szTy = szBOfCTy cTy
	    val szEb = sum(List.map szBOfCTy eb)
	    in
	       if szTy + szEb = 8
		  then eightBytesOfCTys(cTys, [], (cTy :: eb) :: ebs)
	       else if szTy + szEb < 8
	          then eightBytesOfCTys(cTys, cTy :: eb, ebs)
	       else eightBytesOfCTys(cTys, [cTy], eb :: ebs)
	    end

  (* convert a C type into its eight bytes *)
    fun eightBytesOfCTy cTy = eightBytesOfCTys (CTypes.flattenCTy cTy, [], [])

  (* classify a C type into its location kind (assuming that aggregates cannot be passed in registers) *)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = K_FPR
      | kindOfCTy (CTy.C_ARRAY _ | CTy.C_STRUCT _ | CTy.C_UNION _) = raise Fail "impossible"
      | kindOfCTy (CTy.C_unsigned _ | CTy.C_signed _ | CTy.C_PTR) = K_GPR

    fun combineKinds (k1, k2) = if (k1 = k2)
	then k1
	else (case (k1, k2)
	       of (K_MEM, _) => K_MEM
		| (_, K_MEM) => K_MEM
		| (K_GPR, _) => K_GPR
		| (_, K_GPR) => K_GPR
		| _ => K_FPR
 	      (* end case*))

  (* this part of the ABI is tricky. if the eightbyte contains all floats, we use fprs, but 
   * otherwise we use gprs. *)
    fun kindOfEightByte [] = raise Fail "impossible"
      | kindOfEightByte [cTy] = kindOfCTy cTy
      | kindOfEightByte (cTy1 :: cTy2 :: cTys) = let
	   val k1 = combineKinds (kindOfCTy cTy1, kindOfCTy cTy2)
	   val k2 = kindOfEightByte(cTy2 :: cTys)
           in
	       combineKinds(k1, k2)
	   end

    fun containsUnalignedFields cTy = (case cTy
        of (CTy.C_STRUCT cTys | CTy.C_UNION cTys) => List.exists containsUnalignedFields cTys
	 | cTy => Int.max(8, szBOfCTy cTy) mod 8 <> 0
        (* end case *))

    fun slotsOfCTy (cTy as (CTy.C_STRUCT _ | CTy.C_UNION _ | CTy.C_ARRAY _)) = 
	   if (szBOfCTy cTy > 2*8 orelse containsUnalignedFields cTy)
	      then List.tabulate (szBOfCTy cTy div 8, fn _ => (8*8, K_MEM, 8))
	      else List.map (fn eb => (8*8, kindOfEightByte eb, 8)) (eightBytesOfCTy cTy)
      | slotsOfCTy cTy = [(8*szBOfCTy cTy, kindOfCTy cTy, alignBOfCTy cTy)]

    fun slotOfCTy cTy = (case slotsOfCTy cTy
			  of [slot] => slot
			   | _ => raise Fail "malformed C type"
			(* end case *))

  (* C location of a staged allocation location *) 
    fun cLocOfStagedAlloc (w, SA.REG (_, r), K_GPR) = CCall.C_GPR (w, r)
      | cLocOfStagedAlloc (w, SA.REG (_, r), K_FPR) = CCall.C_FPR (w, r)
      | cLocOfStagedAlloc (w, SA.BLOCK_OFFSET offB, (K_GPR | K_FPR | K_MEM)) = 
  	   CCall.C_STK (w, T.I.fromInt (wordTy, offB))
      | cLocOfStagedAlloc (w, SA.NARROW (loc, w', k), _) = cLocOfStagedAlloc (w', loc, k)
      | cLocOfStagedAlloc _ = raise Fail "impossible"

  (* given a return type, return the locations for the return values *)
    fun layoutReturn retTy = let
	   val returnStepper = SA.mkStep CCs.returnStages	   
	   in
	      case retTy
  	        of CTy.C_void => ([], NONE, CCs.str0)
		 | retTy => let
		       val (str, locs) = SA.doStagedAllocation(CCs.str0, returnStepper, slotsOfCTy retTy)
		       val {sz, align} = CSizes.sizeOfTy retTy
		       in
		           (List.map cLocOfStagedAlloc locs, SOME {szb=szb, align=align}, str)
		       end
            end

  (* given a store and some parameters, return the C locations for those parameters *)
    fun layoutCall (str, paramTys) = let
	   val callStepper = SA.mkStep CCs.callStages
	   fun doParam (paramTy, (str, paramLocss)) = let
	          val (str', paramLocs) = SA.doStagedAllocation(str, callStepper, slotsOfCTy paramTy)
	          in
	             (str', List.map cLocOfStagedAlloc paramLocs :: paramLocss)
	          end
	   val (str, paramLocss) = List.foldl doParam (str, []) paramTys
           in
	      (List.rev paramLocss, str)
           end

    fun layout {conv, retTy, paramTys} = let
	   val (resLocs, structRetLoc, str) = layoutReturn retTy
	   val (paramLocss, str) = layoutCall(str, paramTys)
	 (* number of bytes allocated for the call *)
	   val frameSzB = SA.find(str, CCs.cCallStk)
	   val argMem = {szb=CSizes.alignAddr(frameSzB, CCs.frameAlignB), align=CCs.frameAlignB}
           in
	      {argLocs=paramLocss, argMem=argMem, structRetLoc=structRetLoc, resLocs=resLocs}
	   end

  (* copy the return value into the result location *)
    fun returnVals resLocs = (case resLocs
         of [] => ([], [])
	  | [CCall.C_GPR (ty, r)] => let
		val resReg = C.newReg ()
	    in
		([T.GPR (T.REG (ty, resReg))],	 
		 [T.COPY (ty, [resReg], [r])])
	    end
	  | [CCall.C_FPR (ty, r)] => let
		val resReg = C.newFreg ()
	    in
		([T.FPR (T.FREG (ty, resReg))],
		 [T.FCOPY (ty, [resReg], [r])])
	    end
         (* end case *))

    fun genCall {name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args} = let
	val {argLocs, argMem, resLocs, structRetLoc} = layout(proto)
	val argAlloc = if ((#szb argMem = 0) orelse paramAlloc argMem)
			then []
			else [T.MV (wordTy, C.rsp, T.SUB (wordTy, spReg, 
			      T.LI (T.I.fromInt (wordTy, #szb argMem))))]
	val (copyArgs, gprUses, fprUses) = CCall.copyArgs(args, argLocs)
       (* the defined registers of the call depend on the calling convention *)
 	val defs = (case #conv proto
            of "ccall" => List.map (gpr o #2) CCs.callerSaveRegs @ List.map fpr CCs.callerSaveFRegs
	     | "ccall-bare" => []
	     | conv => raise Fail (concat [
			"unknown calling convention \"", String.toString conv, "\""
		      ])
            (* end case *))
	val uses = List.map gpr gprUses @ List.map fpr fprUses
	val callStm = T.CALL {funct=name, targets=[], defs=defs, uses=uses, region=mem, pops=0}
	val (resultRegs, copyResult) = returnVals(resLocs)
	val callSeq = argAlloc @ copyArgs @ [callStm] @ copyResult
        in
          {callseq=callSeq, result=resultRegs}
        end

  end (* AMD64SVIDFn *)
