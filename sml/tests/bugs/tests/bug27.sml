(* bug27.sml *)

fun compose [] = (fn x => x) |
    compose (f::fl) x = compose fl (f x);
