(* bug1092.1.sml *)

datatype 'a s1 = S1 of 'a
and s2 = S2;

datatype m = M of n s1
and n = N;

M(S1 N);
