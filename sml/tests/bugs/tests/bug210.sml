(* bug210.sml *)

structure S = struct end;
structure T = struct end;
signature Q = sig sharing S=T end;
