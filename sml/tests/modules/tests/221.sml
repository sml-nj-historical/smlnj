(* 221.sml *)
(* error recovery for unbound type name t *)

signature S1 =
sig
  type s = t
end
