(* test9.sml *)
(* keywords: functor *)

(* derived from mlyaccs.sml (old version) *)

signature SET =
sig
  type a
end;

signature GRAPH =
sig
  structure A : SET
end;

functor F (X : sig type t end) : SET =
struct
  type a = X.t
end;

functor G (Y : sig type s end) : GRAPH =
struct
  type d = Y.s
  structure A = F(struct type t = d end)
end;

structure D = G (struct type s = unit end);
