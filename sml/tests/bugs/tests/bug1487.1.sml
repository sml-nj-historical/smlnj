(* bug1487.1.sml *)

val rec (f : 'a -> 'a) = fn x => f x;
