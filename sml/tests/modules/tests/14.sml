(* test14.sml *)
(* keywords: signature, include *)

(* test include *)

signature S1 =
sig
  type t
  val x : t
end;

signature S2 =
sig
  structure A : S1
  type s
  val y : s * A.t
end;

signature S3 =
sig
  structure B : sig type r end
  include S2
  val z : B.r * A.t * s
end;
