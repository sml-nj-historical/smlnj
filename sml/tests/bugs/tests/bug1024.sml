(* bug1024.sml *)

fun f x = let fun g y = (x, y) in (g 4, g"4") end;
