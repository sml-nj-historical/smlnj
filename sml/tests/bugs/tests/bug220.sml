(* bug220.sml *)

signature S = sig
  type foo
  val f : int -> foo
end;
	      
structure Foo : S = struct
  datatype foo_t = Foo of int
  val f = Foo
end;
