(* bug1182.sml *)

datatype t = INT of int;
fun f (INT n) = INT ~n;
