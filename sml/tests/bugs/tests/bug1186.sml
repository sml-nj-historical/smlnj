(* bug1186.sml *)
(* type of x printed as "?.t" instead of "t" *)

datatype t = A
val x = A;     
