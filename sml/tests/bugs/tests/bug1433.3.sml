(* bug1433.2.sml *)

signature S =
sig
  type t
  structure X : sig eqtype u end where type u = t
  val v : X.u
end;

structure A :> S =
struct
  type t = int
  structure X =
    struct
      type u = t
    end
  val v = 1
end;

(A.v = A.v);
