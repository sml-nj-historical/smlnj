functor CCallFn (
    structure T : MLTREE
    structure C : CELLS
    val offSp : T.I.machine_int -> T.rexp
    val wordTy : int
  ) = struct

    datatype c_arg 
      = ARG of T.rexp	
	  (* rexp specifies integer or pointer; if the 
           * corresponding parameter is a C struct, then 
	   * this argument is the address of the struct. 
	   *)
      | FARG of T.fexp
	  (* fexp specifies floating-point argument *)

    (* kinds of locations for passing C arguments *)
    datatype location_kinds
      = K_GPR                (* general-purpose registers *)
      | K_FPR                (* floating-point registers *)
      | K_MEM                (* memory locations *)

    (* An arg_location specifies the location of arguments/parameters
     * for a C call.  Offsets are given with respect to the low end 
     * of the parameter area. *)
    datatype arg_location =
	C_GPR  of (T.ty * T.reg) (* integer/pointer argument in register *)
      | C_FPR  of (T.fty * T.reg) (* floating-point argument in register *)
      | C_STK  of (T.ty * T.I.machine_int)  (* integer/pointer argument on the call stack *)
      | C_FSTK of (T.fty * T.I.machine_int) (* floating-point argument on the call stack *)

    fun copyToReg (mty, r, e) = let
	val tmp = C.newReg ()
        in
	    [T.COPY (mty, [r], [tmp]), T.MV (mty, tmp, e)]
        end

    fun copyToFReg (mty, r, e) = let
	val tmp = C.newFreg ()
        in
	    [T.FCOPY (mty, [r], [tmp]), T.FMV (mty, tmp, e)] 
        end

    val stack = T.Region.stack

    fun lit i = T.LI (T.I.fromInt (32, i))

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

  end (* CCallFn *)
