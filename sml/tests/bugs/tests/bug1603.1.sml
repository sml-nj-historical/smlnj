(* bug1603.1.sml *)

(* This causes an Unbound exception *)  
signature FOO = sig type hey = string datatype foo = A | B of hey end;

structure Foo = struct type hey = string datatype foo = A | B of hey end;

signature BAR = sig structure Foo : FOO datatype bar = datatype Foo.foo end;

functor Bar() : BAR =
struct
   structure Foo = Foo
   datatype bar = datatype Foo.foo
end;
