(* bug453b.sml *)

structure A = 
struct
  structure A1 = struct exception Bug of unit end
		    (* Bug must have an argument, but it's type doesn't matter,
		       could be int, string, or unit *)
  val x = ()  (* this must be here *)
end;

A.A1.Bug ();


