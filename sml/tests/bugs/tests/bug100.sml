(* bug100.sml *)

signature X = sig datatype T = T end;

structure X: X = struct datatype T = T end;

open X;
