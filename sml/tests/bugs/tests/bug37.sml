(* bug37.sml *)

signature SIG = sig type t val x: t end;
structure S: SIG = struct type t = int val x = 3 end;
S.x;
