(* bug264.sml *)
(* check for multiple type specifications *)

signature X = sig type t val x : t type t end;
