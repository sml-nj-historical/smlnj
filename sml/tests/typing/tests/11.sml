(* 11.sml *)

val rec f = fn (x: 'a) => x : 'a
and g = fn (y: 'a) => (f y):'a;

