(* 36.2.sml *)
(* keywords: functor, equality, sharing *)

signature T6 =
sig
  structure A : sig type t end
  structure B : sig type s end where type s = A.t
end;

functor F(X:T6) =
struct
  val f = fn (x:X.A.t->int,y:X.B.s) => x(y)
end;
