(* test30.sml *)
(* keywords: functor, equality *)

signature S1 =
sig
  type t
  eqtype s
  sharing type t = s
end;

functor F(X:S1) =
struct
  fun f(x:X.t) = (x = x)
end;

