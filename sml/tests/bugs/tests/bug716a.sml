(* bug716a.sml *)
(* 716. overflow on real operation crashes sml on sparc *)

fun bug r = ( r * r; r:real );
bug 1.0E160;  (* core dump! *)
