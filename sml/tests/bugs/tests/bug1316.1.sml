(* bug1316.1.sml *)

fun f x::xs = x+f(xs);
