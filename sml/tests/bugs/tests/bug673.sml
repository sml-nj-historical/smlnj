(* bug673.sml *)
(* example of higher-order functors from Mads Tofte *)

signature MONOID = 
sig
  type t
  val plus: t*t -> t
  val e: t
end;

(* functor signature declaration *)
funsig PROD (structure M: MONOID
	     structure N: MONOID) = MONOID

functor Square(structure X: MONOID
	       functor Prod: PROD): MONOID =
    Prod(structure M = X
         structure N = X);

functor Prod(structure M: MONOID
	     structure N: MONOID): MONOID =
struct
  type t = M.t * N.t
  val e = (M.e, N.e)
  fun plus((m1,n1),(m2,n2))=(M.plus(m1,m2),N.plus(n1,n2))
end;

structure Int =
struct
  type t = int
  val e = 0
  val plus = (op +): int*int -> int
end;

structure Int2 = Square(
  structure X = Int
  functor Prod = Prod);

#1(Int2.e);
