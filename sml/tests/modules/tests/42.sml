(* test42.sml *)
(* keywords: equality, matching *)

signature T =
sig
  eqtype t
end;

structure A : T =
struct
  type t = int -> int
end;

