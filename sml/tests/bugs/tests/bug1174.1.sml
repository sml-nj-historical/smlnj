(* bug1174.1.sml *)

signature SIG = 
sig 
  structure A : sig end
end;

structure B :> SIG = struct end;
