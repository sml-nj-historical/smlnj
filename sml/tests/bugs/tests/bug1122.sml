(* bug1122.sml *)

signature a=sig val b:int end
structure c :> a=struct end;
