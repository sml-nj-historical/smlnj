(* test35.2.sml *)
(* keywords: functor, equality, sharing *)

signature T6 =
sig
  structure A : sig eqtype t end
end;

functor F(X:T6) =
struct
  val f = fn (x:X.A.t) => (x=x)
end;
