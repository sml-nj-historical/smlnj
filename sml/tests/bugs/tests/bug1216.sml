(* bug1216.sml *)

signature SIG = sig end
functor F (A : SIG) (B : SIG) = struct end
structure V = struct end
structure X = F (struct end) (struct end)
structure Y = F (structure A = V) (structure B = V);

structure Z = F V V ; (* this one is problematic *)
