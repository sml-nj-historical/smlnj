(* Compiler bug: PPObj: switch: none of the datacons matched *)

datatype d = D of t
and s = A of int | B of string
and t = C of s;

val p = C(B "a");


