(* 62.1.sml *)
(* keywords: sharing *)

(* Doligez *)
(* sharing with a free structure which does not have the type component *)
(* status : should issue a warning, does not in 66, does in dd67 *)

structure A =
  struct type t = int
  end;

structure B : sig end = A;

signature S =
  sig structure C : sig type u end = B
  end;
