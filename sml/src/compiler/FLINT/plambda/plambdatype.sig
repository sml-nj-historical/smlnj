(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* plambdatype.sig *)

signature PLAMBDATYPE = 
sig

include LTYEXTERN

val lt_merge : lty * lty -> lty

end (* signature PLAMBDATYPE *) 

