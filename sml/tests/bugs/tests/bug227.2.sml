(* bug227.2.sml *)
(* equality on a datatype loops *)

datatype 'a d = D1 | D2 of ('a d * 'a);

D1 = D1;

