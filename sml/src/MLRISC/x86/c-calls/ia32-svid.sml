(* ia32-svid.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * C function calls for the IA32 using the System V ABI.
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
   structure I : X86INSTR
   val ix : ('r,'f) X86InstrExt.sext -> T.sext
     sharing T.LabelExp = I.LabelExp) =
struct
  structure T  = T
  structure Ty = CTypes
  structure C = I.C
  structure IX = X86InstrExt

  fun error msg = MLRiscErrorMsg.error ("X86CompCCalls.", msg)

  (* multiple calling conventions on a single architecture *)
  type calling_convention = unit

  (* prototype describing C function *)
  type  c_proto = 
    { conv : calling_convention,
      retTy : CTypes.c_type,
      paramTys : CTypes.c_type list
     }

  exception ArgParamMismatch

  datatype  c_arg 
    = ARG of T.rexp	
    | FARG of T.fexp
    | ARGS of c_arg list

  local
    fun fpr(sz,f) = T.FPR(T.FREG(sz, f))
    fun gpr(sz,r) = T.GPR(T.REG(sz, r))
    val st0 = C.ST(0)
    fun eax(sz) = [gpr(sz, C.eax)]
    val eax32 = eax(32)
    val pair = gpr(32, C.edx):: eax32

    fun size(Ty.I_char) = 8
      | size(Ty.I_short) = 16
      | size(Ty.I_int) = 32
      | size(Ty.I_long) = 32
      | size(Ty.I_long_long) = 64
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
      | results(Ty.C_unsigned i) = eax(size i)
      | results(Ty.C_signed i) = eax(size i)
      | results(Ty.C_PTR) = eax32
      | results(Ty.C_ARRAY _) = eax32
      | results(Ty.C_STRUCT _) = eax32

    (* Copy (result) registers into fresh temporaries *)
    fun copyOut([], results, stmts) = (results, stmts)
      | copyOut (T.FPR(T.FREG(sz, f))::rest, results, stmts) = let
	  val t = C.newFreg()
	in copyOut(rest, fpr(sz, t)::results, T.FCOPY(sz,[t],[f])::stmts)
	end
      | copyOut (T.GPR(T.REG(sz, r))::rest, results, stmts) = let
	  val t = C.newReg()
	in copyOut(rest, gpr(sz, t)::results, T.COPY(sz,[t],[r])::stmts)
	end
      | copyOut _ = error "copyOut"
  end

  fun genCall{name, proto={conv,retTy,paramTys}, structRet, args} = let
    fun push signed {sz, e} = let
      fun pushl rexp = T.EXT(ix(IX.PUSHL{sz=32, e=rexp}))
      fun signExtend() = pushl(T.CVTI2I(32, T.SIGN_EXTEND, sz, e))
      fun zeroExtend() = pushl(T.CVTI2I(32, T.ZERO_EXTEND, sz, e))
    in if signed then signExtend() else zeroExtend()
    end

    fun push64 rexp = error "push64"

    fun fst32 fexp = error "fst32"
    fun fst64 fexp = error "fst64"
    fun fst80 fexp = error "fst80"

    val signed = push true
    val unsigned = push false

    fun pushArgs ([], [], stmts) = stmts
      | pushArgs (param::r1, arg::r2, stmts) = let
	  fun next stmt = pushArgs (r1, r2, stmt::stmts)
	in
	  case (param, arg)
	  of (Ty.C_float, FARG fexp) => next(fst32 fexp)
	   | (Ty.C_double, FARG fexp) => next(fst64 fexp)
	   | (Ty.C_long_double, FARG fexp) => next(fst80 fexp)

	   | (Ty.C_unsigned(Ty.I_char), ARG rexp) => next(unsigned{sz=8, e=rexp})
	   | (Ty.C_unsigned(Ty.I_short), ARG rexp) => next(unsigned{sz=16, e=rexp})
	   | (Ty.C_unsigned(Ty.I_int), ARG rexp) => next(unsigned{sz=32, e=rexp})
	   | (Ty.C_unsigned(Ty.I_long), ARG rexp) => next(unsigned{sz=32, e=rexp})
	   | (Ty.C_unsigned(Ty.I_long_long), ARG rexp) => next(push64(rexp))

	   | (Ty.C_signed(Ty.I_char), ARG rexp) => next(signed{sz=8, e=rexp})
	   | (Ty.C_signed(Ty.I_short), ARG rexp) => next(signed{sz=16, e=rexp})
	   | (Ty.C_signed(Ty.I_int), ARG rexp) => next(signed{sz=32, e=rexp})
	   | (Ty.C_signed(Ty.I_long), ARG rexp) => next(signed{sz=32, e=rexp})
	   | (Ty.C_signed(Ty.I_long_long), ARG rexp) => next(push64 rexp)
	   | (Ty.C_PTR, ARG rexp) => next(unsigned{sz=32, e=rexp})
	   | (Ty.C_ARRAY _, ARG rexp) => next(unsigned{sz=32, e=rexp})
	   | (Ty.C_STRUCT stuff, ARG rexp) => next(unsigned{sz=32, e=rexp})
	   | (Ty.C_STRUCT params, ARGS args) => 
	        pushArgs(r1, r2, pushArgs(params, args, stmts))
	   | _ => raise ArgParamMismatch
	 (* end case *)
	end
      | pushArgs _ = raise ArgParamMismatch

    (* call defines callersave registers and uses result registers. *)
    fun mkCall ret = let
      val defs = [T.GPR(T.REG(32,C.ecx)), T.GPR(T.REG(32,C.edx))]
      val uses = ret
    in T.CALL{funct=name, targets=[], defs=defs, uses=uses, 
              cdefs=[], cuses=[], region=T.Region.memory}
    end

    val c_rets = results(retTy)
    val (retRegs, cpys) = copyOut(c_rets, [], [])
    val callSeq = pushArgs(paramTys, args, mkCall(c_rets)::cpys)
  in {callseq=callSeq, result=retRegs}
  end
end

     
