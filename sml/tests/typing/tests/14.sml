(* 14.sml *)

fun f() = let val {a,...} = g() in () end
and g() = {a = (), b = ()};

