(* bug1555.1.sml *)

signature S =
sig
  structure T : sig type t end
  structure S : sig type t end
  sharing T = T
end;

