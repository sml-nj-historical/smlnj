(* bug167.sml *)
(* repeated bound type variables in type declaration *)

datatype ('a, 'a, 'a) T = A of 'a | B of 'a;
