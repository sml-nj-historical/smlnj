(* 185.sml *)

signature S =
sig
  type s
  datatype d = D of s
  eqtype t
  sharing type d = t
end;
