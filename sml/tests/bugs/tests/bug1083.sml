(* bug1083.sml *)

datatype foo = A | B of foo;
B A;
