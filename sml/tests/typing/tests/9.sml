(* 9.sml *)
(* in 109.26 caused "Compiler bug: PPType: printTyvar" *)

fun f (x : 'a -> bool) = x 10;
