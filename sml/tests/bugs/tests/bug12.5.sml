(* bug12.5.sml *)

signature SS = sig type r val x : r end;
structure S:SS = struct datatype r = C of int val x = C 3 end;
S.x;
