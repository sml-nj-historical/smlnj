(* bug847.sml *)
(* 847. opening structures with variables conflicting with constructors *)

structure M: sig type t val D:t end = struct type t = int val D = 4 end;

datatype t = D;

open M;
