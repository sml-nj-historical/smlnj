(* bug251.3.sml *)

val rec f = fn x => x + 1
and f = fn n => if n > 0 then 1 else n * f (n - 1);
