(* 263.sml *)

signature S = sig end;
funsig FOO (X:S) (Y:S) = S;
functor Foo (X:S) (Y:S) = X;
functor Foo : FOO = Foo;
structure C = Foo(struct end);


