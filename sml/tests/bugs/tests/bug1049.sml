(* bug1049.sml *)

fun f x = (map real (#1 x); map op o x);
