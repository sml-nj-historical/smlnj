datatype d = D of t
and s = A of int | B of string
and t = C of s list;

val p = C[B "b"];
