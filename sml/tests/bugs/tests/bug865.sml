(* bug865.sml *)

fun f (x : {a : int}) = let fun g x = #a x in g x end;
