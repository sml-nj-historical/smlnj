(* bug242.sml *)
(* forward referencing allowed in sharing constraint *)

signature SIG = sig  type t end;

functor F(structure STR1: sig type t end sharing type STR1.t = Foo.t
	  structure Foo: SIG) =
struct end;

