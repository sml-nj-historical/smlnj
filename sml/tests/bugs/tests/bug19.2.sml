(* bug19.2.sml *)

signature FOO = sig exception Foo of string end;

structure Foo: FOO = struct exception Foo: string end;
