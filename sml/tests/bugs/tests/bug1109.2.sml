(* bug1109.2.sml *)

datatype 'a lyst = NIL of string | CONS of ('a * 'a lyst) ref;
val a = CONS (ref ("foo", NIL "nil"));
