(* bug684.sml *)
(* Compiler bug: checkList *)

signature S = sig end;
funsig FOO (X:S) (Y:S) = S;
functor Foo (X:S) (Y:S) = X;
functor Foo: FOO = Foo;  (* with this deleted produces different strange error *)
structure A:S = struct end;
structure C:S = Foo(A);  (* with this deleted, no error *)
functor Bar5 = Foo(Foo(A)); (* ===> Error: Compiler bug: checkList *)

