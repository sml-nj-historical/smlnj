(* causes memory fault *)

datatype d = D of t
and s = A | B of string
and t = C of s list

val p = C[B "b"];
