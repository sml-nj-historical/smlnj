(* no problem *)

datatype s = A | B of string
and t = C of s list
and d = D of t;

val p = C[B "b"];
