(* bug1139.1.sml *)

val f = 
    let fun g(t,arg as {a = [],e = e}) c = 
	    let val str = arg
	     in ()
	    end
     in ()
    end;
