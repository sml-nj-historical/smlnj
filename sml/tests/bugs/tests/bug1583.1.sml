(* bug1583.1.sml *)

(* This code causes the bug *)
signature FOO = sig type bar = string datatype foo = A | B | C of bar end
structure Foo = struct type bar = string datatype foo = A | B | C of bar end
signature BAR = sig structure Foo : FOO datatype foo = datatype Foo.foo end
functor Bar() : BAR = struct
   structure Foo = Foo
   datatype foo = datatype Foo.foo
end;

(* But this doesn't *)
signature FOO = sig type bar = string datatype foo = A | B | C of string end
structure Foo = struct type bar = string datatype foo = A | B | C of string end
signature BAR = sig structure Foo : FOO datatype foo = datatype Foo.foo end
functor Bar() : BAR = struct
   structure Foo = Foo
   datatype foo = datatype Foo.foo
end;        
                             
