(* bug12.6.sml *)

structure S: sig type r val x : r end =
struct datatype r = C of int val x = C 3 end;

S.x;
