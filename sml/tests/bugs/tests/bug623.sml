(* bug623.sml *)
(* wildcard is equivalent to a serie of wildcards *)

fun foo x 0 = x | foo _ = 0;  (* this failed in pre-0.88 versions *)
