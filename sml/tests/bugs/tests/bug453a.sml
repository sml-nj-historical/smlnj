(* bug453a.sml *)

structure A = 
struct
  structure A1 = struct exception Bug of unit end
		    (* Bug must have an argument, but it's type doesn't matter,
		       could be int, string, or unit *)
  structure A2 = struct end  (* no bug without this, or if this is declared
			        before A1 *)
end;

A.A1.Bug ();


