datatype s = A | B of string
and t = C of s
and d = D of t;

val p = C(B "b");
