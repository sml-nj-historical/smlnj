(* bug705.sml *)
(* sml emacs subprocess dies when file is "used" *)

exception foo;
fun foo'(x) = raise foo;
foo'(4);
