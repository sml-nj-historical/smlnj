
signature PFLATTEN =
sig
    type llty = PLambda.lty
    type ltyc = PLambda.tyc
    type flty = FLINT.lty
    type ftyc = FLINT.tyc
    type lexp = FLINT.lexp
    type value = FLINT.value
    type lvar = FLINT.lvar

    val all_flatten : llty -> (llty list * bool *
			       ((lvar * lexp) -> (lvar list * lexp)) *
			       (value -> (value list * (lexp -> lexp))))

    val ltc_flat    : llty -> llty list
    val ltp_flat    : llty -> bool
    val v_unflatten : llty -> (lvar * lexp) -> (lvar list * lexp)
    val v_flatten   : llty -> value -> (value list * (lexp -> lexp))

    (* recursively turn cooked types into raw when possible *)
    val ltc_raw : llty -> flty
    val tcc_raw : ltyc -> ftyc

end
