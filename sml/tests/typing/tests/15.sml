(* 15.sml *)

fun f() = let val x as {a,...} = g() in () end
and g() = {a = (), b = ()};
