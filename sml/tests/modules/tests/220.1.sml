(* 220.1sml *)
(* error recovery for unbound structure name C *)

signature S1 =
sig
  structure A : sig end
end where A = C;
