(* 63.1.sml *)
(* keywords: sharing *)

(* Doligez *)
(* sharing with a free structure which does not have the substructure *)
(* status : should issue a warning, does not in 66, does in dd67 *)

structure A =
struct
  type t = int
end;

signature S =
sig
  structure C : sig structure D : sig end end = A
end;
