(* bug957.sml *)

val f = fn x => let val b = fn (y:'_a) => y in x b end;
