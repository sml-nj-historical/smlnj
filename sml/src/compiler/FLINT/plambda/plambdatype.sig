(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* plambdatype.sig *)

signature PLAMBDATYPE = 
sig

include LTYEXTERN

val lt_merge : lty * lty -> lty
val tcc_arw  : tyc * tyc -> tyc
val ltc_funN : LtyKernel.rawflag * lty list * lty list -> lty
val lt_pinst : lty * tyc list -> lty
val ltd_pfun : lty -> lty * lty

end (* signature PLAMBDATYPE *) 

