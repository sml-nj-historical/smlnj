(* ia32-svid.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * C function calls for the IA32 using the System V ABI
 *
 * Register conventions:
 *
 *    %eax	return value
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
functor IA32SVID_CCalls
  (structure T : MLTREE
   val ix : (T.stm,T.rexp,T.fexp,T.ccexp) X86InstrExt.sext -> T.sext
  ) : C_CALL =
struct
  structure T  = T
  structure Ty = CTypes
  structure C = X86Cells
  structure IX = X86InstrExt

  fun error msg = MLRiscErrorMsg.error ("X86CompCCalls", msg)

  (* multiple calling conventions on a single architecture *)
  type calling_convention = unit

  (* prototype describing C function *)
  type  c_proto = 
    { (* conv : calling_convention, *)
      retTy : CTypes.c_type,
      paramTys : CTypes.c_type list
     }

  exception ArgParamMismatch

  datatype  c_arg 
    = ARG of T.rexp	    
    | FARG of T.fexp
    | ARGS of c_arg list

  val mem = T.Region.memory
  val stack = T.Region.memory

  fun intSize(Ty.I_char) = 8
    | intSize(Ty.I_short) = 16
    | intSize(Ty.I_int) = 32
    | intSize(Ty.I_long) = 32
    | intSize(Ty.I_long_long) = 64

  fun sizeOf(Ty.C_void) = 32
    | sizeOf(Ty.C_float) = 32
    | sizeOf(Ty.C_double) = 64
    | sizeOf(Ty.C_long_double) = 80	(* no padding required *)
    | sizeOf(Ty.C_unsigned i) = intSize i
    | sizeOf(Ty.C_signed i) = intSize i
    | sizeOf(Ty.C_PTR) = 32
    | sizeOf(Ty.C_ARRAY _) = 32
    | sizeOf(Ty.C_STRUCT fields) = structSz fields

  and structSz fields = 
    List.foldl  (fn (fld, sum) => sizeOf fld + sum) 0 fields

  val sp = C.esp

  local
    fun fpr(sz,f) = T.FPR(T.FREG(sz, f))
    fun gpr(sz,r) = T.GPR(T.REG(sz, r))
    val st0 = C.ST(0)
    fun eax(sz) = [gpr(sz, C.eax)]
    val eax32 = eax(32)
    val pair = gpr(32, C.edx):: eax32
  in
   (* List of result registers; 
    * Multiple returns have most significant register first.
    *)
    fun results(Ty.C_void) = []
      | results(Ty.C_float) = [fpr(32, st0)]
      | results(Ty.C_double) = [fpr(64, st0)]
      | results(Ty.C_long_double) = [fpr(80, st0)]
      | results(Ty.C_unsigned(Ty.I_long_long)) = pair
      | results(Ty.C_signed(Ty.I_long_long)) =  pair
      | results(Ty.C_unsigned i) = eax(intSize i)
      | results(Ty.C_signed i) = eax(intSize i)
      | results(Ty.C_PTR) = eax32
      | results(Ty.C_ARRAY _) = eax32
      | results(Ty.C_STRUCT _) = eax32

    fun fstp (sz, f)  =
      (case sz
       of 32 => T.EXT(ix(IX.FSTPS(f)))
        | 64 => T.EXT(ix(IX.FSTPL(f)))
	| 80 => T.EXT(ix(IX.FSTPT(f)))
      (*esac*))

    (* Copy (result) registers into fresh temporaries *)
    fun copyOut([], results, stmts) = (results, stmts)
      | copyOut (T.FPR(T.FREG(sz, f))::rest, results, stmts) = let
	  val t = C.newFreg()
	in copyOut(rest, fpr(sz, t)::results, 
		   fstp(sz, T.FREG(sz,t))::stmts)
	end
      | copyOut (T.GPR(T.REG(sz, r))::rest, results, stmts) = let
	  val t = C.newReg()
	in copyOut(rest, gpr(sz, t)::results, T.COPY(sz,[t],[r])::stmts)
	end
      | copyOut _ = error "copyOut"
  end

  fun genCall{name, proto={retTy, paramTys}, structRet, args} = let
    fun push signed {sz, e} = let
      fun pushl rexp = T.EXT(ix(IX.PUSHL(rexp)))
      fun signExtend(e) = 
	if sz=32 then e else T.CVTI2I(32, T.SIGN_EXTEND, sz, e)
      fun zeroExtend(e) = 
	if sz=32 then e else T.CVTI2I(32, T.ZERO_EXTEND, sz, e)
    in 
      pushl(if signed then signExtend(e) else zeroExtend(e))
    end
    val signed = push true
    val unsigned = push false

    fun push64 rexp = error "push64"

    (* increment the stack pointer and store floating point result. *)
    fun bumpSp sz = T.MV(32, sp, T.SUB(32, T.REG(32,sp), T.LI sz))
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
	    val sz = Int.quot(structSz fields, 8)
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
		  fun ea() = T.ADD(32, ptr, T.LI i)
		  fun fload (bits, bytes) =
		    mkArgs(rest, i+bytes, 
			   FARG(T.FLOAD(bits, ea(), mem))::acc)
		  fun load (bits, bytes) =
		    mkArgs(rest, i+bytes, 
			   ARG(T.LOAD(bits, ea(), mem))::acc)
		  fun intSz(cint) = let
		    val sz = intSize cint
		  in (sz, Int.quot(sz, 8))
		  end
		in
		  case ty
		  of Ty.C_void => error "STRUCT: void field"
		   | Ty.C_float => fload(32, 4)
		   | Ty.C_double => fload(64, 8)
		   | Ty.C_long_double => fload(80, 10)
		   | Ty.C_unsigned(cint) => load(intSz(cint))
		   | Ty.C_signed(cint) => load(intSz(cint))
		   | Ty.C_PTR => load(32, 8)
		   | Ty.C_ARRAY _ => load(32, 8)
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
		       | (Ty.C_float, FARG fexp)	 => cont(pushf(32,fexp))
		       | (Ty.C_double, FARG fexp)	 => cont(pushf(64, fexp))
		       | (Ty.C_long_double, FARG fexp)   => cont(pushf(80, fexp))
		       | (Ty.C_unsigned(cint), ARG rexp) => pushCint(cint, rexp)
		       | (Ty.C_signed(cint), ARG rexp)   => pushCint(cint, rexp)
		       | (Ty.C_PTR, ARG rexp)		 => pushl(rexp)
		       | (Ty.C_ARRAY _, ARG rexp)	 => pushl(rexp) 
		       | (Ty.C_STRUCT fields, ARG rexp)  => let
			   val (ldPtr, args) = mkStructArgs(fields, rexp)
			 in cont(ldPtr::pushStruct(fields, args, stmts))
			 end
		       | (Ty.C_STRUCT fields, ARGS rexps) => let
			 in cont(pushStruct(fields, rexps, stmts))
			 end
		    end (* pushStruct *)
	     in pushArgs(r1, r2, pad16(fields, pushStruct(fields, args, stmts)))
	     end
	   | _ => raise ArgParamMismatch
	 (* end case *)
	end
      | pushArgs _ = raise ArgParamMismatch

    (* struct return address is an implicit 0th argument*)
    fun pushStructRetAddr (acc) = 
     (case retTy
      of Ty.C_STRUCT fields => let
	   val sz = structSz fields
	   val addr = structRet{szb=Int.quot(sz, 8), align=0}
	 in unsigned{sz=32, e=addr}::acc
	 end
       | _ => acc
     (*esac*))

    (* call defines callersave registers and uses result registers. *)
    fun mkCall ret = let
      val defs = [T.GPR(T.REG(32,C.ecx)), T.GPR(T.REG(32,C.edx))]
      val uses = ret
    in T.CALL{funct=name, targets=[], defs=defs, uses=uses, 
              cdefs=[], cuses=[], region=T.Region.memory}
    end

    val c_rets = results(retTy)
    val (retRegs, cpyOut) = copyOut(c_rets, [], [])
    val call = mkCall(c_rets)::cpyOut
    val callSeq = pushArgs(paramTys, args, pushStructRetAddr(call))
  in {callseq=callSeq, result=retRegs}
  end
end

     
