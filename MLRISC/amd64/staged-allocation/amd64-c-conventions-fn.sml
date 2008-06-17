(* amd64-c-conventions-fn.sml
 * 
 * C function calls for the AMD64 using the System V ABI
 * 
 * Register conventions:
 *   %rax            return value             (caller save)
 *   %rbx            optional base pointer    (callee save)
 *   %rbp            optional frame pointer   (callee save)
 *   %rdi            arg 1                    (caller save)
 *   %rsi            arg 2                    (caller save)
 *   %rdx            arg 3                    (caller save)
 *   %rcx            arg 4                    (caller save)
 *   %r8             arg 5                    (caller save)
 *   %r9             arg 6                    (caller save)
 *   %r10            chain pointer            (caller save)
 *   %r11            scratch                  (caller save)
 *   %r12-r15        general purpose          (callee save)
 *   %xmm0-xmm1      pass/return fp           (caller save)
 *   %xmm2-xmm7      pass fp                  (caller save)
 *   %xmm8-xmm15     scratch                  (caller save)
 *
 * Calling conventions:
 * 
 *    Return result:
 *      + Integer and pointer results are returned in %rax.
 *      + 128-bit integers returned in %rax/%rdx.
 *      + Floating-point results returned in %xmm0/%xmm1.
 *      + Small structs returned in %rax/%rdx. Otherwise, returned
 *        in space provided by the caller.
 *
 *    Function arguments:
 *      + The first 6 integer arguments go in the argument registers.
 *      + The first 8 floating-point registers go in %xmm0-xmm8.
 *      + Otherwise, arguments are pushed on the stack right to left.
 *      + The stack is 16-byte aligned.
 *      + Struct arguments are padded out to word length.
 *)


functor AMD64CConventionsFn (
    type reg

  (* relevant GPRs *)
    val rax : reg
    val rdi : reg
    val rsi : reg
    val rdx : reg
    val rcx : reg
    val r8 : reg
    val r9 : reg
  (* relevant FPRs*)
    val xmm0 : reg
    val xmm1 : reg
    val xmm2 : reg
    val xmm3 : reg
    val xmm4 : reg
    val xmm5 : reg
    val xmm6 : reg
    val xmm7 : reg

    structure SA : STAGED_ALLOCATION
          where type reg = reg
          where type location_kind = CLocationKinds.location_kind

  ) = struct

    datatype location_kind = datatype CLocationKinds.location_kind

    fun toRegInfo r = (64, r)
    fun toRegInfos rs = List.map toRegInfo rs

  (* registers *)
    val gprReturnRegs = [rax, rdx]
    val fprReturnRegs = [xmm0, xmm1]

    val gprParamRegs = [rdi, rsi, rdx, rcx, r8, r9]
    val fprParamRegs = [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7]

  (* staged-allocation counters *)
    val cCallStk = SA.freshCounter ()
    val cCallGpr = SA.freshCounter ()
    val cCallFpr = SA.freshCounter ()
    val (cRetGpr, ssGpr) = SA.useRegs (toRegInfos gprReturnRegs)
    val (cRetFpr, ssFloat) = SA.useRegs (toRegInfos fprReturnRegs)

  (* frame alignment in bytes *)
    val maxAlign = 16

  (* initial store *)
    val str0 = SA.init [cCallStk, cCallGpr, cCallFpr, cRetFpr, cRetGpr]

  (* rules for passing arguments *)
    val callStages = [ 
	  SA.CHOICE [
	    (fn (w, k, str) => k = K_GPR, SA.SEQ [
		SA.WIDEN (fn w => Int.max (64, w)),
		SA.BITCOUNTER cCallGpr,
		SA.REGS_BY_BITS (cCallGpr, toRegInfos gprParamRegs) 
	    ]),
	    (fn (w, k, str) => k = K_FPR, SA.SEQ [
		SA.WIDEN (fn w => Int.max (64, w)),
		SA.BITCOUNTER cCallFpr,
		SA.REGS_BY_BITS (cCallFpr, toRegInfos fprParamRegs) 
	    ]),
	    (fn (w, k, str) => k = K_MEM,
	     SA.OVERFLOW {counter=cCallStk, 
			  blockDirection=SA.UP, 
			  maxAlign=maxAlign}) 
	  ],
	  SA.OVERFLOW {counter=cCallStk, 
		       blockDirection=SA.UP, 
		       maxAlign=maxAlign}
    ]

  (* rules for returning values *)
    val returnStages = [
	  SA.CHOICE [
	    (fn (w, k, str) => k = K_GPR,
	     SA.SEQ [SA.WIDEN (fn w => Int.max (64, w)), ssGpr]),
	    (fn (w, k, str) => k = K_FPR,
	     SA.SEQ [SA.WIDEN (fn w => Int.max (64, w)), ssFloat])]
	  ]

  end
