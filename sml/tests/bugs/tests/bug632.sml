(* bug632.sml *)
(* minimal or maximal integer literals in patterns cause compiler bug *)

fun foo 0x20000000 = 5;

fun foo ~0x20000001 = 5; 
