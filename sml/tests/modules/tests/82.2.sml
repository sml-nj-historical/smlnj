(* 82.2.sml *)

signature S1 =
sig
  structure A : sig type t end
end
where A =Integer
and A = Real;

