(* bug272.sml *)

fun f(x) = let val y : 'a -> 'a = x in y y end;

f (fn x: 'a => x);
