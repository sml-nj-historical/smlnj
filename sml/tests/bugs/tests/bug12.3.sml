(* bug12.3.sml *)

structure S = struct type t = int  datatype r = C of t end;
S.C;
S.C 3;
