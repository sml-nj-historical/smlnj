(* bug450.sml *)

signature I = sig type T end;
structure J :> I = struct type u = int end;
