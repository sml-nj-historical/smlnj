(* bug43.sml *)
(*
    "unbound structure id in sharing spec" error message was reporting the
    wrong structure id.
*)

signature SIG = sig end;

signature SIG' = sig
  structure S:SIG
end;

(* Here it complained that S' is unbound in the sharing specification, but
actually it's S'.T that is unbound! *)

signature SIG'' = sig
  structure S':SIG'
  structure T:SIG
  sharing S'.T = T
end;
