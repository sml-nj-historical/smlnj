(* bug319.sml *)
(* 319. bad weakness degree (not weak enough) *)

fun f x = let fun g z = ref x in g 3 end;

val r = f [];
