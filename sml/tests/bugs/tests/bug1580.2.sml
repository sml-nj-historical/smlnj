(* bug1580.2.sml *)

fun vector2List v = Vector.foldr (fn (e,l) => (e::l)) [] v;

fun concatVectors v = Vector.concat (vector2List v);

fun f d = concatVectors #[concatVectors (Vector.map f #[])];

