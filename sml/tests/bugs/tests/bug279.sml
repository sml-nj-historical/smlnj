(* bug279.sml *)

fun printi i = print(Int.toString i)
fun f (i : int, two_i : int) : unit =
    (printi i; print ": "; printi two_i; print "\n"; f(i+1, two_i+two_i));

f(0, 1);
