(* bug717.sml *)
(* 717. records always evaluated in alphabetical order *)

val _ = {b= print "1", a = print "2"};

