(* bug412.sml *)
(* raised Runbind *)

structure A =
   struct
     val x = 5
     structure B = 
	struct
	   val y = 5
	end
   end

open A.B;
structure A = struct end;
y;
