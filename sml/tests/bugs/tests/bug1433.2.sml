(* bug1433.2.sml *)

signature S =
sig
  type t
  structure X : sig eqtype u end where type u = t
  val v : X.u
end;
