(* bug1603.2.sml *)

(* But this is okay *)
signature FOO = sig type hey = string datatype foo = A | B of string end;
structure Foo = struct type hey = string datatype foo = A | B of string end;
signature BAR = sig structure Foo : FOO datatype bar = datatype Foo.foo end;
functor Bar() : BAR =
struct
   structure Foo = Foo
   datatype bar = datatype Foo.foo
end;
