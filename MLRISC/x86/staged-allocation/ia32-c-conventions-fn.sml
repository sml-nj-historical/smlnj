(* ia32-c-conventions-fn.sml
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

functor IA32CConventionsFn (
    type reg
    val eax : reg
    val edx : reg
    val st0 : reg
    structure SA : STAGED_ALLOCATION
           where type reg = reg
           where type location_kind = CLocationKinds.location_kind
  ) = struct

  (* conventions for calling a C function *)
    val alignB = 16
    val cStack = SA.freshCounter()
    val callStages = [
	SA.SEQ[
	SA.WIDEN (fn ty => Int.max(32, ty)),
	SA.OVERFLOW {counter=cStack, blockDirection=SA.UP, maxAlign=alignB}
	]
    ]

  (* conventions for returning from a C call *)
    val (cInt, retInGpr) = SA.useRegs [(32, eax), (32, edx)]
    val (cFloat, retInFpr) = SA.useRegs [(80, st0)]
    val returnStages = [
  	  SA.CHOICE [
	  (* return in general-purpose register *)
	  (fn (ty, k, str) => k = CLocationKinds.K_GPR,
	   SA.SEQ [SA.WIDEN (fn ty => Int.max(ty, 32)),
		   retInGpr]),
	  (* return in floating-point register *)
	  (fn (ty, k, str) => k = CLocationKinds.K_FPR,
	   SA.SEQ [SA.WIDEN (fn ty => 80), retInFpr])
	]
    ]
		       
  (* initial store *)
    val str0 = SA.init [cInt, cFloat, cStack]
	       
end (* IA32CConventionsFn *)
