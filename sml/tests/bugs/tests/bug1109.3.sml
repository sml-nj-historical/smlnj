(* bug1109.3.sml *)

datatype foo = A | B of foo ref;
B(ref A);
