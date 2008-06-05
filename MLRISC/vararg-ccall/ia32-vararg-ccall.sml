val fast_floating_point = ref true

structure DummyRegion =
struct
   type region = unit
   fun toString () = "" 
   val memory = ()
   val stack = ()
   val readonly = ()
   val spill = ()
end

structure DummyExtension =
struct

   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) X86InstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit

end

structure DummyConst =
struct
   type const = unit
   fun toString() = ""  
   fun hash() = 0w0  
   fun valueOf _ = 0
   fun == _ = true  
end

structure X86MLTree =
   MLTreeF (structure Constant  = DummyConst
    structure Region    = DummyRegion
    structure Extension = DummyExtension)

structure CCall = IA32SVIDFn(
                     structure T = X86MLTree
		     val abi = ""
		     val ix = fn x => x
		     val fast_floating_point = fast_floating_point
		  )


structure IA32VarargCCall =
  struct

    structure CTy = CTypes
    structure Consts = VarargCCallConstants
    structure V = VarArgs
    structure CB = CellsBasis
    structure T = X86MLTree

    val wordTy = 32

    val regToInt = CB.physicalRegisterNum

    fun argToCTy (V.I _) = CTy.C_signed CTy.I_int
      | argToCTy (V.R _) = CTy.C_double
      | argToCTy (V.B _) = CTy.C_signed CTy.I_int
      | argToCTy (V.S _) = CTy.C_PTR

  (* runtime friendly representation of the C location *)
    fun encodeCLoc (CCall.C_GPR (ty, r)) = (Consts.GPR, regToInt r, ty)
      | encodeCLoc (CCall.C_FPR (ty, r)) = (Consts.FPR, regToInt r, ty)
      | encodeCLoc (CCall.C_STK (ty, off)) = (Consts.STK, T.I.toInt (wordTy, off), ty)
      | encodeCLoc (CCall.C_FSTK (ty, off)) = (Consts.FSTK, T.I.toInt (wordTy, off), ty)

  (* takes a vararg and a location and returns the vararg triplet *)
    fun varArg (arg, loc) = let
	   val (k, l, ty) = encodeCLoc loc
           in
	     (arg, k, l, ty)
	   end

  (* package the arguments with their locations *)
    fun zipArgs args = let
	    val argTys = List.map argToCTy args
	    val {argLocs, argMem, ...} = CCall.layout {conv="c-call", retTy=CTy.C_void, paramTys=argTys}
	  (* expect single locations, as we do not pass aggregates to vararg functions *)
	    val argLocs = List.map List.hd argLocs
            in
  	        (ListPair.mapEq varArg (args, List.rev argLocs), argMem)
	    end

  (* align the frame to 16 bytes *)
    fun darwinStkSzB stkArgSzB = let
	   val retAndFrameSzB = 2*4
	   val stkAllocSzB = stkArgSzB + retAndFrameSzB
	   val stkAllocSzB = IA32CSizes.alignAddr(stkAllocSzB, 16)
	   val stkAllocSzB = stkAllocSzB - retAndFrameSzB
           in
	       stkAllocSzB
	   end

    fun callWithArgs (cFun, args) = let
	   val (zippedArgs, {szb, ...}) = zipArgs args
	   in
	      VarargCCall.vararg(cFun, zippedArgs, darwinStkSzB szb)
	   end

  end

