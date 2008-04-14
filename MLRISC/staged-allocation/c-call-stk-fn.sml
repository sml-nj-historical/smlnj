(* c-call-stk-fn.sml
 *
 * Common functions for machines that pass arguments in registers and 
 * on the stack.
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor CCallStkFn (
    structure C : CELLS
    structure T : MLTREE
    val spReg : T.rexp
    val wordTy : T.ty
  )  = 
  struct

    structure CTy = CTypes
    val stack = T.Region.stack

    datatype c_arg =
	     (* rexp specifies integer or pointer; if the 
              * corresponding parameter is a C struct, then 
	      * this argument is the address of the struct. 
	      *)
	     ARG of T.rexp	
	   (* fexp specifies floating-point argument *)		    
	   | FARG of T.fexp
	   (* list of arguments corresponding to the contents of a C struct *)		     
	   | ARGS of c_arg list

    (* An arg_location specifies the location of arguments/parameters
     * for a C call.  Offsets are given with respect to the low end 
     * of the parameter area. *)
    datatype arg_location =
	C_GPR  of (T.ty * T.reg) (* integer/pointer argument in register *)
      | C_FPR  of (T.fty * T.reg) (* floating-point argument in register *)
      | C_STK  of (T.ty * T.I.machine_int)  (* integer/pointer argument on the call stack *)
      | C_FSTK of (T.fty * T.I.machine_int) (* floating-point argument on the call stack *)
		     
    fun offSp 0 = spReg
      | offSp offset = T.ADD (wordTy, spReg, T.LI offset)

    (* copy the arguments into the parameter locations *)
    fun copyArgs' ([], [], stms, gprs, fprs) = (List.rev stms, gprs, fprs)
      | copyArgs' (arg :: args, loc :: locs, stms, gprs, fprs) = let
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
		  copyArgs' (args, locs, stms, gprs, fprs)
		end
      | copyArgs' _ = raise Fail "argument arity error"

    fun copyArgs (args, argLocs) = copyArgs'(args, argLocs, [], [], [])

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

  end (* CCallFn *)
