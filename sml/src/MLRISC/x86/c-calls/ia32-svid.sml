(* ia32-svid.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
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
 *	+ Integer and pointer results are returned in %eax
 *	+ 64-bit integers (long long) returned in %eax/%edx
 *	+ Floating point results are returned in %st(0) (all types).
 *	+ Struct results are returned in space provided by the caller.
 *	  The address of this space is passed to the callee as an
 *	  implicit 0th argument, and on return %eax contains this
 *	  address.
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
 * Questions:
 *    - what about stack frame alignment?
 *)

functor IA32SVID_CCalls (
    structure T : MLTREE
    val ix : (T.stm,T.rexp,T.fexp,T.ccexp) X86InstrExt.sext -> T.sext
  (* Note that the fast_loating_point flag must match the one passed
   * to the code generator module.
   *)
    val fast_floating_point : bool ref
  ) : C_CALLS = struct

    structure T  = T
    structure Ty = CTypes
    structure C = X86Cells
    structure IX = X86InstrExt

    fun error msg = MLRiscErrorMsg.error ("X86CompCCalls", msg)

    datatype  c_arg 
      = ARG of T.rexp	    
      | FARG of T.fexp
      | ARGS of c_arg list

    val mem = T.Region.memory
    val stack = T.Region.memory

    val paramAreaOffset = 0 (* stack offset to param area *)

  (* This annotation is used to indicate that a call returns a fp value 
   * in %st(0) 
   *)
    val fpReturnValueInST0 = #create MLRiscAnnotations.RETURN_ARG C.ST0

  (* map C integer types to their MLRisc type *)
    fun intTy (Ty.I_char) = 8
      | intTy (Ty.I_short) = 16
      | intTy (Ty.I_int) = 32
      | intTy (Ty.I_long) = 32
      | intTy (Ty.I_long_long) = 64

  (* size in bytes of C integer type *)
    fun intSize (Ty.I_char) = 1
      | intSize (Ty.I_short) = 2
      | intSize (Ty.I_int) = 4
      | intSize (Ty.I_long) = 4
      | intSize (Ty.I_long_long) = 8

  (* size in bytes of C type *)
    fun sizeOf (Ty.C_void) = 4
      | sizeOf (Ty.C_float) = 4
      | sizeOf (Ty.C_double) = 8
      | sizeOf (Ty.C_long_double) = 12	(* no padding required *)
      | sizeOf (Ty.C_unsigned i) = intSize i
      | sizeOf (Ty.C_signed i) = intSize i
      | sizeOf (Ty.C_PTR) = 4
      | sizeOf (Ty.C_ARRAY _) = 4
      | sizeOf (Ty.C_STRUCT fields) = structSz fields

  (* size in bytes of C struct type *)
    and structSz fields = 
	  List.foldl  (fn (fld, sum) => sizeOf fld + sum) 0 fields

    val sp = C.esp
    fun LI i = T.LI(T.I.fromInt(32, i))

    local
      fun fpr(sz,f) = T.FPR(T.FREG(sz, f))
      fun gpr(sz,r) = T.GPR(T.REG(sz, r))
      val st0 = C.ST(0)
    (* note that the caller saves includes the result register (%eax) *)
      val callerSaves = [gpr(32, C.eax), gpr(32, C.ecx), gpr(32, C.edx)]
      val oneRes = [gpr(32, C.eax)]
      val twoRes = [gpr(32, C.edx), gpr(32, C.eax)]
    in
  (* List of registers defined by a C Call; this is the result registers
   * plus the caller save registers.
   * Multiple returns have most significant register first.
   *)
    fun resultsAndDefs (Ty.C_void) = ([], callerSaves)
      | resultsAndDefs (Ty.C_float) =
	  ([fpr(32, st0)], fpr(32, st0) :: callerSaves)
      | resultsAndDefs (Ty.C_double) =
	  ([fpr(64, st0)], fpr(64, st0) :: callerSaves)
      | resultsAndDefs (Ty.C_long_double) =
	  ([fpr(80, st0)], fpr(80, st0) :: callerSaves)
      | resultsAndDefs (Ty.C_unsigned(Ty.I_long_long)) =
	  (twoRes, gpr(32, C.edx) :: callerSaves)
      | resultsAndDefs (Ty.C_signed(Ty.I_long_long)) =
	  (twoRes, gpr(32, C.edx) :: callerSaves)
      | resultsAndDefs (Ty.C_unsigned i) = (oneRes, callerSaves)
      | resultsAndDefs (Ty.C_signed i) = (oneRes, callerSaves)
      | resultsAndDefs (Ty.C_PTR) = (oneRes, callerSaves)
      | resultsAndDefs (Ty.C_ARRAY _) = (oneRes, callerSaves)
      | resultsAndDefs (Ty.C_STRUCT _) = (oneRes, callerSaves)

    fun fstp (32, f) = T.EXT(ix(IX.FSTPS(f)))
      | fstp (64, f) = T.EXT(ix(IX.FSTPL(f)))
      | fstp (80, f) = T.EXT(ix(IX.FSTPT(f)))

  (* Copy (result) registers into fresh temporaries *)
    fun copyOut([], results, stmts) = (results, stmts)
      | copyOut (T.FPR(T.FREG(sz, f))::rest, results, stmts) = let
	  val t = C.newFreg()
        (* If we are using fast floating point mode then do NOT 
         * generate FSTP.
         * --- Allen 
         *)
	  val stmt = if !fast_floating_point 
        	then T.FCOPY(sz, [t], [f])
        	else fstp(sz, T.FREG(sz, t))
	  in
	    copyOut (rest, fpr(sz, t)::results, stmt::stmts)
	  end
      | copyOut (T.GPR(T.REG(sz, r))::rest, results, stmts) = let
	  val t = C.newReg()
	  in
	    copyOut(rest, gpr(sz, t)::results, T.COPY(sz,[t],[r])::stmts)
	  end
      | copyOut _ = error "copyOut"
    end (* local *)

    fun genCall ar = let
	  val {
		name, proto, paramAlloc, structRet,
		saveRestoreDedicated, callComment, args
	      } = ar
	  val {conv, retTy, paramTys} = proto
	  val calleePops = (case conv
		 of (""|"ccall") => false
		  | "stdcall" => true
		  | _ => error (concat [
			"unknown calling convention \"", String.toString conv, "\""
		    ])
		(* end case *))
	  fun push signed {sz, e} = let
		fun pushl rexp = T.EXT(ix(IX.PUSHL(rexp)))
		fun signExtend(e) = if sz=32 then e else T.SX(32, sz, e)
		fun zeroExtend(e) = if sz=32 then e else T.ZX(32, sz, e)
		in 
		  pushl(if signed then signExtend(e) else zeroExtend(e))
		end
	  val signed = push true
	  val unsigned = push false

	  fun push64 rexp = error "push64"
	  (* increment the stack pointer and store floating point result. *)
	  fun bumpSp sz = T.MV(32, sp, T.SUB(32, T.REG(32,sp), LI sz))
	  fun storeAtSp(sz, e) = T.STORE(sz, T.REG(32,sp), e, stack)
	  fun PUSHB(e, stmts) = bumpSp(1)::storeAtSp(8, e)::stmts
	  fun PUSHW(e, stmts) = bumpSp(2)::storeAtSp(16, e)::stmts

	  fun fst32 fexp = [bumpSp(4), T.FSTORE(32, T.REG(32, sp), fexp, stack)]
	  fun fst64 fexp = [bumpSp(8), T.FSTORE(64, T.REG(32, sp), fexp, stack)]
	  fun fst80 fexp = [bumpSp(10), T.FSTORE(80, T.REG(32, sp), fexp, stack)]

	  fun pushArgs ([], [], stmts) = stmts
	    | pushArgs (param::r1, arg::r2, stmts) = let
		fun next stmt = pushArgs (r1, r2, stmt::stmts)
		fun nextL stmt = pushArgs (r1, r2, stmt@stmts)
		(* struct arguments are padded to word boundaries. *)
		fun pad16(fields, stmts) = let
		  val sz = structSz fields
        	in
		  case Word.andb(Word.fromInt sz, 0w1)
		   of 0w0 => stmts
		    | 0w1 => bumpSp(1)::stmts
		  (*esac*)
		end
		fun mkStructArgs(fields, rexp) = let
		  val ptrR = C.newReg()
		  val ptr = T.REG(32, ptrR)
		  fun mkArgs([], i, acc) = (i, rev acc)
		    | mkArgs(ty::rest, i, acc) = let
			fun ea() = T.ADD(32, ptr, LI i)
			fun fload (bits, bytes) =
			  mkArgs(rest, i+bytes, 
				 FARG(T.FLOAD(bits, ea(), mem))::acc)
			fun load (bits, bytes) =
			  mkArgs(rest, i+bytes, 
				 ARG(T.LOAD(bits, ea(), mem))::acc)
			fun intSz cint = (intTy cint, intSize cint)
		      in
			case ty
			of Ty.C_void => error "STRUCT: void field"
			 | Ty.C_float => fload(32, 4)
			 | Ty.C_double => fload(64, 8)
			 | Ty.C_long_double => fload(80, 10)
			 | Ty.C_unsigned(cint) => load(intSz(cint))
			 | Ty.C_signed(cint) => load(intSz(cint))
			 | Ty.C_PTR => load(32, 4)
			 | Ty.C_ARRAY _ => load(32, 4)
			 | Ty.C_STRUCT fields => let
			     val (i, args) = mkArgs(fields, i, [])
			   in mkArgs(rest, i, ARGS args::acc)
			   end
		      end
		in (T.MV(32, ptrR, rexp), #2 (mkArgs(fields, 0, [])))
		end
	      in
		case (param, arg)
		of (Ty.C_float, FARG fexp) => nextL(fst32 fexp)
		 | (Ty.C_double, FARG fexp) => nextL(fst64 fexp)
		 | (Ty.C_long_double, FARG fexp) => nextL(fst80 fexp)
		 | (Ty.C_unsigned(Ty.I_char), ARG rexp) => 
		      next(unsigned{sz=8, e=rexp})
		 | (Ty.C_unsigned(Ty.I_short), ARG rexp) => 
		      next(unsigned{sz=16, e=rexp})
		 | (Ty.C_unsigned(Ty.I_int), ARG rexp) => 
		      next(unsigned{sz=32, e=rexp})
		 | (Ty.C_unsigned(Ty.I_long), ARG rexp) => 
		      next(unsigned{sz=32, e=rexp})
		 | (Ty.C_unsigned(Ty.I_long_long), ARG rexp) => 
		      next(push64(rexp))
		 | (Ty.C_signed(Ty.I_char), ARG rexp) => next(signed{sz=8, e=rexp})
		 | (Ty.C_signed(Ty.I_short), ARG rexp) => next(signed{sz=16, e=rexp})
		 | (Ty.C_signed(Ty.I_int), ARG rexp) => next(signed{sz=32, e=rexp})
		 | (Ty.C_signed(Ty.I_long), ARG rexp) => next(signed{sz=32, e=rexp})
		 | (Ty.C_signed(Ty.I_long_long), ARG rexp) => next(push64 rexp)
		 | (Ty.C_PTR, ARG rexp) => next(unsigned{sz=32, e=rexp})
		 | (Ty.C_ARRAY _, ARG rexp) => next(unsigned{sz=32, e=rexp})
		 | (Ty.C_STRUCT fields, ARG rexp) => let
		      val (ldPtr, args) = mkStructArgs(fields, rexp)
		      val stmts = pushArgs([param], [ARGS(args)], stmts)
		   in pushArgs(r1, r2, ldPtr::stmts)
        	   end
		 | (Ty.C_STRUCT fields, ARGS args) => let
		      fun pushStruct([], [], stmts) = stmts
			| pushStruct(ty::tys, arg::args, stmts) = let
			    fun cont(stmts) = pushStruct(tys, args, stmts)
			    fun pushf(sz, fexp) = 
			      (case sz
			       of 32 => fst32(fexp)
				| 64 => fst64(fexp)
				| 80 => fst80(fexp)
			       (*esac*)) @ stmts
			    fun pushb (rexp) = cont(PUSHB(rexp, stmts))
			    fun pushw (rexp) = cont(PUSHW(rexp, stmts))
			    fun pushl (rexp) = cont(T.EXT(ix(IX.PUSHL(rexp)))::stmts)
			    fun pushCint(cint, rexp) = 
			     (case cint
			      of Ty.I_char => pushb(rexp)
			       | Ty.I_short => pushw(rexp)
			       | Ty.I_int => pushl(rexp)
			       | Ty.I_long => pushl(rexp)
			       | Ty.I_long_long => error "STRUCT: long_long"
			     (*esac*))
			  in
			    case (ty, arg)
			    of (Ty.C_void, _) => error "STRUCT: void field"
			     | (Ty.C_float, FARG fexp) => cont(pushf(32,fexp))
			     | (Ty.C_double, FARG fexp)	=> cont(pushf(64, fexp))
			     | (Ty.C_long_double, FARG fexp) =>
				  cont(pushf(80, fexp))
			     | (Ty.C_unsigned(cint), ARG rexp) =>
				  pushCint(cint, rexp)
			     | (Ty.C_signed(cint), ARG rexp) => pushCint(cint, rexp)
			     | (Ty.C_PTR, ARG rexp) => pushl(rexp)
			     | (Ty.C_ARRAY _, ARG rexp)	 => pushl(rexp) 
			     | (Ty.C_STRUCT fields, ARG rexp)  => let
				 val (ldPtr, args) = mkStructArgs(fields, rexp)
				 in cont(ldPtr::pushStruct(fields, args, stmts))
				 end
			     | (Ty.C_STRUCT fields, ARGS rexps) =>
				  cont(pushStruct(fields, rexps, stmts))
			  end (* pushStruct *)
		      in
		        pushArgs(r1, r2,
			  pad16(fields, pushStruct(fields, args, stmts)))
		      end
		 | _ => error "argument/parameter mismatch"
	       (* end case *)
	      end
	    | pushArgs _ = error "argument/parameter mismatch"

	  (* struct return address is an implicit 0th argument*)
	  fun pushStructRetAddr (acc) = (case retTy
		 of Ty.C_STRUCT fields => let
		      val addr = structRet{szb=structSz fields, align=0}
		      in
			unsigned{sz=32, e=addr}::acc
		      end
		  | _ => acc
		(* end case *))

	(* call defines callersave registers and uses result registers. *)
	  fun mkCall (defs, npop) = let
	      val npop = Int32.fromInt npop
	      val { save, restore } = saveRestoreDedicated defs
	      val callstm = 
		  T.CALL { funct=name, targets=[], defs=defs, uses=[], 
			   region=T.Region.memory, pops=npop }
	      val callstm =
		  case callComment of
		      NONE => callstm
		    | SOME c => T.ANNOTATION (callstm,
					      #create MLRiscAnnotations.COMMENT c)
	    (* If return type is floating point then add an annotation RETURN_ARG 
	     * This is currently a hack.  Eventually MLTREE *should* support
	     * return arguments for CALLs.
	     * --- Allen
	     *)
              fun markFpReturn callstm = T.ANNOTATION(callstm, fpReturnValueInST0)
              val callstm =
        	  if !fast_floating_point then
        	     (case retTy
                       of Ty.C_float => markFpReturn callstm
        	     | Ty.C_double => markFpReturn callstm
        	     | Ty.C_long_double => markFpReturn callstm
        	     |  _ => callstm
        	     )
        	  else callstm
	  in
	      save @ callstm :: restore
	  end

	(* size to pop off on return *)
	  fun argsSz(Ty.C_STRUCT fields::rest) = let
        	val sz = structSz fields
		fun pad16 bytes = (case Word.andb(Word.fromInt sz, 0w1)
		       of 0w0 => sz
			| 0w1 => sz+1
		      (* end case *))
        	in
		  pad16 sz + argsSz(rest)
        	end
	    | argsSz(ty::rest) =
	      (* remember that char and short get promoted... *)
	        Int.max(sizeOf(ty),4)+argsSz(rest)
	    | argsSz [] = 0

	  val (cRets, cDefs) = resultsAndDefs (retTy)
	  val (retRegs, cpyOut) = copyOut(cRets, [], [])
	  val n = argsSz paramTys
	  val (popseq, implicit_pop) =
	      if calleePops orelse n = 0 then ([], n)
	      else ([T.MV(32, sp, T.ADD(32, T.REG(32,sp), LI n))], 0)
	  val call = mkCall(cDefs, implicit_pop) @ popseq @ cpyOut
	  val callSeq = pushArgs(paramTys, args, pushStructRetAddr(call))
	  in
	    {callseq=callSeq, result=retRegs}
	  end (* genCall *)

  end

