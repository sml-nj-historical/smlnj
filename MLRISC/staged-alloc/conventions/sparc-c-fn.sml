(* sparc-c-fn.sml
 *
 * C calling convention for the Sparc.
 *
 *)
functor SparcCConventionFn (
    type reg_id

  (* parameter GPRs*)
    val r8 : reg_id
    val r9 : reg_id
    val r10 : reg_id
    val r11 : reg_id
    val r12 : reg_id
    val r13 : reg_id
  (* parameter FPRs *)
    val f0 : reg_id
    val f1 : reg_id
    val f8 : reg_id

    structure SA : STAGED_ALLOCATION
          where type reg_id = reg_id
          where type loc_kind = CLocKind.loc_kind


  ) = struct

    datatype loc_kind = datatype CLocKind.loc_kind

    fun gpr r = (32, GPR, r)
    fun gprs rs = List.map gpr rs
    fun fpr r = (64, FPR, r)
    fun fprs rs = List.map fpr rs

    val useRegs = #2 o SA.useRegs

  (* conventions for calling a C function *)
    val cStack = SA.freshCounter()
    val params = [
	  SA.WIDEN (fn w => Int.max(32, w)),
	  useRegs (List.map gpr [r8, r9, r10, r12, r13]),
	  SA.OVERFLOW{counter=cStack, blockDirection=SA.UP, maxAlign=8}
	]

  (* rules for returning values *)
    val returns = [
	  SA.WIDEN (fn w => Int.max(32, w)),
	  SA.CHOICE [
	    (fn (w, k, store) => k = FPR, useRegs (List.map fpr [f0, f1])),
	    (fn (w, k, store) => true, useRegs (List.map gpr [f8]))
	  ]
	]


  end
