(* 262.sml *)

signature S = sig end;
functor Foo (X:S) (Y:S) = X;
structure C = Foo(struct end);


