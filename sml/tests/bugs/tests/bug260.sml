(* bug260.sml *)

fun exp 0 = 1 | exp i = 2 * exp (i-1);

val a = exp 29;
val minint = ~a + ~a;

(* should raise overflow *)

val test = minint div ~1;
