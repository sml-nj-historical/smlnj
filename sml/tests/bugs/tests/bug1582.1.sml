(* bug1582.1.sml *)

fun f x = f(x*x);
f 2;
