(* bug12.4.sml *)

signature SS = sig type t datatype r = C of t end;
structure S:SS = struct type t = int  datatype r = C of t end;
S.C;
S.C 3;
