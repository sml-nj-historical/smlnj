(* bug1580.1.sml *)

datatype a = A1 | A2 of a
datatype b = B1 | B2 of b vector

fun f A1    = B1
  | f(A2 a) = B2(#[f a])
