(* bug1084.sml *)

hd nil;

fun f x = f x;

f 0;  (* this will loop, until the bug is fixed *)
