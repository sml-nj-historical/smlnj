(* bug868a.sml *)
(* 868. "Compiler bug: distributeT: dist_abv" caused by type abbrev in sign *)

functor F(S : sig type t = int end) = struct end;
