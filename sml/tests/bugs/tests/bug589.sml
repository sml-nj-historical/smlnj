(* bug589.sml *)
(* occurs check & nonstrict type abbreviations *)

type 'a CON = int;

fun foo (x:'a) = (3:'a CON);

fun bar x = bar (foo x);

(* The last line should not hang the type checker *)
