(* bug936.sml *)
(* Compiler bug: CoreInfo.coreLty3 *)

fun foo x = 
    let abstype t1 = T | F
	with
	  fun goo y = foo T
	end
     in x
    end;
