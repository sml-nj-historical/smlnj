(* bug857.sml *)

fun f x = (h x)+0.1  (* this is the bug *)
and g x = (h x)+1
and h (x) : int = 0;
