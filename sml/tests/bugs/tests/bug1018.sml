(* bug1018.sml *)
(* should cause Overflow *)

val x = ~1073741824;
x - 1;
