(* test46.sml *)
(* keywords: functor, sharing, equality *)

(* derived from bug109.sml *)

signature EQSIG =
sig
  type r
  datatype s = S of r
  sharing type s = r
end;

functor F(X : EQSIG) =
struct
  fun test(x : X.s) = (x = x);
end;
