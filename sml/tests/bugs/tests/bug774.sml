(* bug774.sml *)

exception A;
exception B = A;
val one = case A of B => 1 | A => 2;
