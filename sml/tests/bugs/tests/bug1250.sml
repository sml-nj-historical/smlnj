(* bug1250.sml *)

datatype 'a foo = Foo1 of 'a | Foo2 of 'a
val Foo1 f = Foo2(fn x => x);
f 13;
