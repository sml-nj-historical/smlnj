(* 220.sml *)
(* error recovery for unbound structure name C *)

signature S1 =
sig
  structure A : sig end = C
end
