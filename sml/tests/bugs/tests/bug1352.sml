(* bug1352.sml *)

fun f (x,y) = (x=y,x/y);
f (2.0, 2.0);
