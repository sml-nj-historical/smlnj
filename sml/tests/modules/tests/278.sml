(* 278.sml *)
(* sharing and where type *)

signature S =
sig
  type s
  type t
  sharing type s = t
end;

(* S1 should succeed *)
signature S1 = S where type t = int * int;

functor F(X: S1) =
struct
  val x : X.s = (1,2)
end;
