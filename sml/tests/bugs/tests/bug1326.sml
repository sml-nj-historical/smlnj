(* bug1326.sml *)

fun f x _ = []
  | f x _ = let val y = g ()
	    in  1
	    end;
