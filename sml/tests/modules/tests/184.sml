(* 184.sml *)

signature S =
sig
  datatype d = D of int -> int
  eqtype t
  sharing type d = t
end;
