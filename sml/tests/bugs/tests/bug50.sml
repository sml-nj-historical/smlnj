(* bug50.sml *)
(*
  Free references to a sibling structure in a signature were not allowed.
*)

signature SS =
sig
  structure A : sig type t end
  structure B : sig val x : A.t end
end
