(* bug393.sml *)

type ('a,'b) bogus = 'a;
val x = 0 : (int,int) bogus; (* really int *)
val y = 1 : (int,int list) bogus; (* really int *)
fun f (x:'a,y:'a) = ();  (* force the types to unify *)
f (x,y);  (* unify (int,int) bogus and (int,int list) bogus *)
	  (* should be equivalent to unifying int with int  *)
	  (* but it's not                                   *)
