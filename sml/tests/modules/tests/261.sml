(* 261.sml *)

signature S = sig end;
functor Foo (X:S) (Y:S) = X;
functor C = Foo(struct end);


