(* test41.sml *)
(* keywords: equality, sharing *)

signature S1 =
sig
  datatype d = D of int -> int
  eqtype s
  sharing type d = s
end;
