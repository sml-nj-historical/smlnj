(* causes memory fault *)

datatype d = D of t
and s = A | B of string
and t = C of s;

val p = C(B "a");
