(* test35.sml *)
(* keywords: functor, equality, sharing *)

signature T6 =
sig
  structure A : sig type t end
  structure B : sig eqtype s sharing type s = A.t end
end;

functor F(X:T6) =
struct
  val f = fn (x:X.A.t) => (x=x)
end;
