(* sparc-c-call-fn.sml
 *
 * C function calls for the Sparc
 *
 *)

functor SparcCCallFn (
    structure T : MLTREE
    val ix : (T.stm, T.rexp, T.fexp, T.ccexp) SparcInstrExt.sext
	         -> T.sext
  ) = struct

    structure Ty = CTypes
    structure C = SparcCells
    structure IX = SparcInstrExt
    structure CTy = CTypes

    val wordTy = 32

    val GP = C.GPReg
    val FP = C.FPReg

    fun greg r = GP r
    fun oreg r = GP (r + 8)
    fun ireg r = GP (r + 24)
    fun freg r = FP r

    fun reg32 r = T.REG (32, r)
    fun freg64 r = T.FREG (64, r)

    val sp = oreg 6
    val spReg = reg32 sp

    structure CCall = CCallFn(
             structure T = T
	     structure C = C
	     val wordTy = wordTy
	     fun offSp 0 = spReg
	       | offSp offset = T.ADD (32, spReg, T.LI offset))

    datatype c_arg = datatype CCall.c_arg
    datatype arg_location = datatype CCall.arg_location
    datatype loc_kind = datatype CLocKind.loc_kind

    structure SA = StagedAllocationFn (
                    type reg_id = T.reg
		    datatype loc_kind = datatype loc_kind
		    val memSize = 8)

    structure CCs = SparcCConventionFn (
		      type reg_id = T.reg
		      val r8 = oreg 0
		      val r9 = oreg 1
		      val r10 = oreg 2
		      val r11 = oreg 3
		      val r12 = oreg 4
		      val r13 = oreg 5
		      val f0 = freg 0
		      val f1 = freg 1
		      structure SA = SA)

  (* assign a C type to a kind of machine location *)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = FPR
      | kindOfCTy (CTy.C_unsigned _ | CTy.C_signed _ | CTy.C_PTR | CTy.C_ARRAY _) = GPR

  (* takes a C type and a request for passing values of that type *)
    fun cTyToReqs cTy = let
	  val {sz, align} = SparcCSizes.sizeOfTy cTy
          in
	    case cTy
	     of CTy.C_STRUCT _ => raise Fail "todo"
	      | (CTy.C_unsigned CTy.I_long_long |
		 CTy.C_signed CTy.I_long_long   ) => raise Fail "todo"
	      | _ => 
		(* flatten the C type and produce a list of requests *)
		List.map (fn cTy => (sz * 8, kindOfCTy cTy, align)) (CTy.flattenCTy cTy)
	  end

  end
