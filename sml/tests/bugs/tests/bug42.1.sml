(* bug42.1.sml *)
(*
   (1) missing substructures found in environment,
*)

signature SIG = sig type t val x:t end;

signature SIG' = sig structure S:SIG val y:S.t end;

structure T : SIG = struct type t=int val x = 3 end;

structure T' : SIG' = struct structure S=T val y=S.x end;

(* This yields a sensible error message, then an uncaught exception Bind. *)
structure T'' : SIG' = struct val y=T.x end;

