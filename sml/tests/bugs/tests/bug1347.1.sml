(* bug1347.1.sml *)

signature INNER =
sig
  datatype a = A
  and b = B of a
end;

signature OUTER =
sig
  structure Inner : INNER
  datatype b = datatype Inner.b
end;

functor Test(structure Inner : INNER
	     structure Outer : OUTER
	     sharing Outer.Inner = Inner)
    =
struct
  (* Both functions should typecheck but the second one does not. *)
  fun good (Inner.B(Inner.A)) = 5
  fun bad  (Outer.B(Inner.A)) = 5
end;
