(* test40.sml *)
(* keywords: functor, equality, sharing *)

signature T6 =
sig
  structure A : sig type t end
  type s
  sharing type s = A.t
end;

functor F(X:T6) =
struct
  val f = fn (x:X.A.t->int,y:X.s) => x(y)
end;
