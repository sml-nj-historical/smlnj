(* bug932.1.sml *)

signature S = sig type 'a t = 'a list end;
structure T : S = struct type 'a t = 'a list end;
