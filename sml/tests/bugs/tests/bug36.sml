(* bug36.sml *)

fun f x = x + x
and g() = f 1;

fun g() = f 1
and f x = x + x;
