(* bug1163.2.sml *)

datatype dt = A | B;

fun f (A,A) = 0
  | f (B,B) = 1
  | f ((A|B),(A|B)) = 2;
