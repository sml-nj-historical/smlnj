(* bug1584.1.sml *)

fun derive(f,dx) = fn x => (f(x + dx) - f(x)) / dx;
