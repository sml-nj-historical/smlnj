(* bug636.sml *)
(* Vector patterns don't work at top level *)

val v = #[1,2,3];
val #[a,b,c] = v;
a;
