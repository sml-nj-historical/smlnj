(* bug156.sml *)

datatype d = A of int | B of bool;

fun f(A _) = true
  | f(C _) = false;
