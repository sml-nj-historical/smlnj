(* bug1209.sml *)

fun subst (f : 'a -> unit) = Unsafe.cast f ();
