(* bug138.sml *)

val a = {1=0,2=0,3=0,4=0,5=0,6=0,7=0,8=0,9=0};
(*  val a = (0,0,0,0,0,0,0,0,0) : int * ... * int *)      (* OK *)

val b = {1=0,2=0,3=0,4=0,5=0,6=0,7=0,8=0,9=0,10=0};
(* val b = {1=0,10=0,2=0,3=0,4=0,5=0,6=0,7=0,8=0,9=0} : 
	  {1:int,10:int,2:int,3:int,4:int,5:int,6:int,7:int,8:int,9:int}

The resulting record type will not unify with the corresponding tuple
*)
a = (0,0,0,0,0,0,0,0,0);
(* val it = true : bool *)			(* OK *)

b = (0,0,0,0,0,0,0,0,0,0);
(*
Error: operator and operand don't agree (tycon mismatch)
operator domain:{1:int,10:int,2:int,3:int,4:int,5:int,6:int,7:int,8:int,9:int}
* {1:int,10:int,2:int,3:int,4:int,5:int,6:int,7:int,8:int,9:int}
operand:        {1:int,10:int,2:int,3:int,4:int,5:int,6:int,7:int,8:int,9:int}
* (int * int * int * int * int * int * int * int * int * int)
 in expression:
   b = (0,0,0,0,0,0,0,0,0,0)
*)
