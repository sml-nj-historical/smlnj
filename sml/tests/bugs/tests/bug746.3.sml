(* bug746.3.sml *)

datatype ('a,'b) Foo = FOO1 of 'b | FOO2 of unit | FOO3 of 'a * ('a,'b) Foo;
FOO2();

datatype ('a,'b) Foo = FOO1 of 'b | FOO2 of unit;
FOO2();
