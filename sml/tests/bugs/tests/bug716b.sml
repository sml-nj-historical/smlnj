(* bug716b.sml *)
(* 716. overflow on real operation crashes sml on sparc *)

fun bug r = ( r /0.0; r );
bug 1.0;  (* core dump! *)
