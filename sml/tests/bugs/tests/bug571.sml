(* bug 571.sml *)
(* failure to do occurs check when dealing with flex records *)

fun foo {x=x, y=y, z=(z as {...})} = foo z;

(* The above expression should not hang the type checker *)
