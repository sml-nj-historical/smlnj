(* bug1109.1.sml *)

datatype 'a lyst = NIL | CONS of ('a * 'a lyst) ref;
val a = CONS (ref ("foo", NIL));
