(* bug67.sml:  won't parse "fn {x: ty} => x" *)

val _ = fn {x:int} => x;
