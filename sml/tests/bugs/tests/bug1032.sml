(* bug1032.sml *)

signature S =
sig
  structure A : sig type t val y : t end
  val x : A.t
end;
