(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ltynorm.sig *)

signature LTYNORM =
sig 

exception TCENV
exception TeUnbound

(** testing equivalence of tkinds, tycs, ltys, fflags, and rflags *)
val tk_eqv   : Lty.tkind * Lty.tkind -> bool
val tc_eqv   : Lty.tyc * Lty.tyc -> bool
val lt_eqv   : Lty.lty * Lty.lty -> bool
val ff_eqv   : Lty.fflag * Lty.fflag -> bool
val rf_eqv   : Lty.rflag * Lty.rflag -> bool

(** finding out the depth for a tyc's innermost-bound free variables *)
val tc_depth : Lty.tyc * DebIndex.depth -> DebIndex.depth
val tcs_depth: Lty.tyc list * DebIndex.depth -> DebIndex.depth
val tc_nvars : Lty.tyc -> Lty.tvar list
val lt_nvars : Lty.lty -> Lty.tvar list

(** utility functions for TC_ENV and LT_ENV types *)
val tcc_env  : Lty.tyc * int * int * Lty.tycEnv -> Lty.tyc
val ltc_env  : Lty.lty * int * int * Lty.tycEnv -> Lty.lty

(** reducing a tyc or lty into the weak-head normal form *)
val tc_whnm : Lty.tyc -> Lty.tyc
val lt_whnm : Lty.lty -> Lty.lty

(** reducing a tyc or lty into the true normal form *)
val tc_norm : Lty.tyc -> Lty.tyc
val lt_norm : Lty.lty -> Lty.lty

(** automatically flattening the argument or the result type *)
val lt_autoflat : Lty.lty -> bool * Lty.lty list * bool

(** testing if a tyc is a unknown constructor *)
val tc_unknown : Lty.tyc -> bool 

(** automatically tupling up the multiple argument/result into a single one *)
val tc_autotuple : Lty.tyc list -> Lty.tyc

(** tcc_arw does automatic argument and result flattening, so go away *)
val tcc_arw : Lty.fflag * Lty.tyc list * Lty.tyc list -> Lty.tyc

(** primitive TC_WRAP constructor, built through the token facility *)
val wrap_token    : Lty.token

(* normalizing out projections for tyc and lty *)
val tc_out_nm   : Lty.tyc -> Lty.tycI
val lt_out_nm   : Lty.lty -> Lty.ltyI

end (* signature LTYNORM *)
