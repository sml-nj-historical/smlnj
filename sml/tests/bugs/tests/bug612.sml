(* bug612.sml *)

fun id x = x;

(fn y => id (let val u = y in u end)) 1 2;
