(* bug1100.sml *)

signature SET =
sig
  structure Key : sig type key end
  type item = Key.key
end;

signature FOO =
sig
  structure StringSet : SET
  where type Key.key = string
end;

functor FooFun(structure StringSet : SET
	       where type Key.key = string	(*XXX*)
		 ) : FOO =
struct
  structure StringSet = StringSet
end;
