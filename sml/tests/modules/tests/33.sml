(* test33.sml *)
(* keywords: functor, equality, sharing *)

(* This is greatly simplified from the Agent system from Edinburgh. *)

signature S =
sig
    type t
    eqtype s
    sharing type t=s
end;

functor F(S:S) =
struct
  fun f(x : S.t, y : S.t) = (x=y)
end;
