(* bug926.sml *)

Array.extract (Array.fromList ["a","b"],1,SOME 0);
