(* ia32-vararg-ccall-fn.sml
 *
 * C calling conventions for the X86. We use the technique of Staged Allocation (see
 * MLRISC/staged-allocation).
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor IA32VarargCCallFn (
    structure T : MLTREE
    val abi : string
    val ix : (T.stm,T.rexp,T.fexp,T.ccexp) X86InstrExt.sext -> T.sext
  (* Note that the fast_loating_point flag must match the one passed
   * to the code generator module.
   *)
    val fast_floating_point : bool ref
    val push : T.rexp -> T.stm
    val leave : T.stm
  ) = struct

    val wordTy = 32

    structure T = T
    structure C = X86Cells
    structure CB = CellsBasis
    structure CTy = CTypes
    structure CCall = IA32SVIDFn(
                       structure T = T
		       val abi = abi
		       val ix = ix
		       val fast_floating_point = fast_floating_point
		     )
    structure VarargCCall = VarargCCallFn(
			      structure T = T
			      structure CCall = CCall
			      val gprParams = []
			      val fprParams = []
			      val gprTys = [32]
			      val fprTys = [32, 64]
			      val spReg = CCall.spReg
			      val wordTy = wordTy
			      val newReg = C.newReg
			    )

    fun callWithArgs (cFun, args) = let
	   val (triplets, stkArgSzB) = VarargCCall.encodeArgs args
         (* the extra two words are for the return pointer and the frame pointer *)
	   val stkAllocSzB = #szb stkArgSzB + 2*4
	   val stkAllocSzB = IA32CSizes.alignAddr(stkAllocSzB, 16)
	   in
	      raise Fail "jump to the interpreter"
	   end

    fun lit i = T.LI (T.I.fromInt (wordTy, i))

  (* get the ith argument in the calling sequence *)
    fun getArg i = T.LOAD(wordTy, T.ADD(wordTy, T.REG(wordTy, C.ebp), lit (4*i+8)), T.Region.memory)

(*
    fun libh s = let
	
	val sh = DynLinkage.lib_symbol (DynLinkage.main_lib, s)
    in
	fn () => let val addr = DynLinkage.addr sh in print(Word32.toString addr^"\n"); addr end
    end

	fun va () = let
	    val varargAddr = (libh "rand")()
	in 
	    RawMemInlineT.rawccall (varargAddr, (), [])
	end

structure DL = DynLinkage

    fun main's s = let
	val lh = DynLinkage.open_lib
			 { name = "./vararg.so", global = true, lazy = true }
    in 
	DL.lib_symbol (lh, s)
    end
    val malloc_h = main's "vararg"

    fun test () =
	let val w_p = RawMemInlineT.rawccall :
		      Word32.word * Word32.word * (unit * word -> string) list
		      -> Word32.word
	    val a = w_p (DL.addr malloc_h, 0w0, [])
	in 
	    print (Word32.toString a^"\n")
	end
*)

    fun genVarargs () = let
  	   val lab = Label.global "varargs"
	   val args = C.newReg()
	   val cFun = C.newReg()
           in
	      (lab,
	       List.concat [
		   [push (T.REG(wordTy, C.ebp)),
		    T.COPY (wordTy, [C.ebp], [C.esp])],
		   [T.MV(wordTy, cFun, getArg 0)],
		   [T.MV(wordTy, args, getArg 1)],
		 (* allocate stack space for the arguments *)
		   [T.MV(wordTy, C.esp, T.SUB(wordTy, T.REG(wordTy, C.esp), getArg 2))],
	           VarargCCall.genVarargs (T.REG(wordTy, cFun), args),
		   [leave],
		   [T.RET []]
		   ])
	   end	    

  end
