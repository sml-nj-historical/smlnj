(* bug572.sml *)
(* problems with generalizations due to flex records *)

fun snd {a,b} = b;

fun f (x as {a,...}) =
	let val u = snd x
	in
	    u
	end;

f {a=0, b=true};

(* the above should have type bool *)
