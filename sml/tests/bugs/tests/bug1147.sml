(* bug1147.sml *)

signature S = sig type T end;   
signature S' = S where type T = int;
