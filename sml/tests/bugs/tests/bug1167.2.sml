(* bug1167.2.sml *)

datatype exp = UNARY of (string * int) * int
val deco = ("--", 3)
val unary = fn x => (fn (e : int) => UNARY(x, e))
val deco = unary deco
