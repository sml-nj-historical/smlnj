(* bug1110.sml *)

datatype lyst = NIL of string | CONS of (string * lyst) ref;
val a = CONS (ref ("foo", NIL "nil"));
Compiler.Control.Print.printDepth := 100;
Compiler.Control.Print.printLength := 100;
val a = CONS (ref ("foo", NIL "nil"));

