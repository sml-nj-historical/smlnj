(* bug1057.sml *)
(* typo in error message *)

fun foo{x,y,...} = x + size y;
