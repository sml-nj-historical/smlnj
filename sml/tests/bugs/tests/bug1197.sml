(* bug1197.sml *)

signature ABSYN =
sig
  datatype expr = Int of int
end;

signature INTERP =
sig
  structure AbSyn : ABSYN
  val evaluate : AbSyn.expr -> AbSyn.expr
end;

functor Interp (structure A : ABSYN) :>
	INTERP where type AbSyn.expr = A.expr =
struct
  structure AbSyn = A
  open AbSyn
  fun evaluate (Int n) = Int n
end;

structure Absyn : ABSYN =
struct
  datatype expr = Int of int
end;

structure E = Interp(structure A = Absyn);

val x = E.evaluate(Absyn.Int 3);
