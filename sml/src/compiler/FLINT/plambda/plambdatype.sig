(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* plambdatype.sig *)

signature PLAMBDATYPE = 
sig

include LTYEXTERN

val lt_merge : lty * lty -> lty
val tcc_arw  : tyc * tyc -> tyc
val ltc_funN : LtyKernel.fflag * lty list * lty list -> lty
val ltd_pfun : lty -> lty * lty

end (* signature PLAMBDATYPE *) 

