(* test35.1.sml *)
(* keywords: functor, equality, sharing *)

signature T6 =
sig
  structure A : sig eqtype t end
  structure B : sig type s sharing type s = A.t end
end;

functor F(X:T6) =
struct
  val f = fn (x:X.B.s) => (x=x)
end;
