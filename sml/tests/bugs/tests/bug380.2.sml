(* bug380.2.sml *)
 
val rec exposeBug = fn 
  (t,s)::r =>
       (TextIO.output(TextIO.stdOut, 
	  (if s = String.size t then "foo" 
	   else Int.toString (s - String.size t))); 
	exposeBug r );
