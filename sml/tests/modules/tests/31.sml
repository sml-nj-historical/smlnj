(* test31.sml *)
(* keywords: functor, equality, sharing *)

signature T1 =
sig
  type t1
  eqtype t2
  sharing type t1 = t2
  val x : t1
end;

functor F(X: T1) =
struct
  val a = (X.x = X.x)
end;
