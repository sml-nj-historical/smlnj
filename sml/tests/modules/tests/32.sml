(* test32.sml *)
(* keywords: functor, equality *)

(* this one should work even without sharing propagation *)

signature S1 =
sig
  type t
  eqtype s
  sharing type t = s
end;

functor F(X:S1) =
struct
  fun f(x:X.s) = (x = x)
end;


