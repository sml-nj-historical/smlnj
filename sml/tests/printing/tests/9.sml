(* Error: Compiler bug: PPObj: switch: none of the datacons matched *)

datatype d = D of t
and s = A of int | B of int
and t = C of s;

val p = C(B 0);
