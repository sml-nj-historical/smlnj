(* bug1191.2.sml *)

datatype d = D | E  (* d must have more than one constructor *)
and h = H of d; (* must be pseudo recursive ("and", not "datatype" *)

datatype h = datatype h;

fun f (H _) = true;
fun g (H _) = true | g _ = false;

val a = H(D);
val g_a = g a;
val f_a = f a;

