(* bug47.1.sml *)
(*
  some uses of user-bound type variables have strange effects
*)

fun f(x) = let val y : 'a = x in y y end;
f 3;
