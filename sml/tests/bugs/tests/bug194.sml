(* bug194.sml *)
(* weak type variable syntax: '1a == '_a *)

val x = fn (a,b) => (ref a; ref (a b));
val y = x : ('1a -> '1b) * '1a -> '1b ref;
val y = x : ('1a -> '1b) * '_a -> '_b ref;
