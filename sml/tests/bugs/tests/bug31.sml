(* bug31.sml *)

structure S = struct type t = int; val x = 1 end;
open S;
structure S = struct type t = bool; val x = true end;
x;
