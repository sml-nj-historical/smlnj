(* bug1241.sml *)

datatype 'a t1 = A1 | B1 of ('a list)
val i1 = A1;

datatype 'a t2 = A2 of int | B2 of ('a list)
val i2 = A2 0;
