(* bug746.2.sml *)

datatype foo = Foo of unit | Bar of int * foo;
Foo;
Foo();
