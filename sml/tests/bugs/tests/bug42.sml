(* bug42.sml *)

signature SIG = sig type t val x:t end;

signature SIG' = sig structure S:SIG val y:S.t end;

structure T : SIG = struct type t=int val x = 3 end;

structure T' : SIG' = struct structure S=T val y=S.x end;

(* This yields a sensible error message, then an uncaught exception Bind. *)
structure T'' : SIG' = struct val y=T.x end;

signature SIG'' = sig structure T:SIG val y:T.t end;

(* This should not succeed, but it does!  The unbound structure appears
   in the global environment, so it doesn't notice that the substructure T
   is missing.
*)
structure U : SIG'' = struct val y = T.x end;
