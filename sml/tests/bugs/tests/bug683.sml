(* bug683.sml *)
(* Compiler bug: Extern: update_structure 2 *)

signature S =
sig
  type t
  functor Foo(X:S) : S
end;
