(* bug155.sml *)
(* compiler bug caused by missing substructure *)

signature S1 =
sig
  type t
end;

signature S2 =
sig
  structure A : S1
  val x : A.t
end;

structure B : S2 =
struct
  val x = 3
end;
