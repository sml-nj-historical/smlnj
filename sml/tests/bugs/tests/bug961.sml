(* bug961.sml *)
(* include type overlaying *)

signature s = sig type t end;
signature ss = sig include s; datatype t = T end;
