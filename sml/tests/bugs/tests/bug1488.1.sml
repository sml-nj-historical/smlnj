(* bug1488.1.sml *)

val ls : (int * int) list = [];
val arr = Array.fromList ls;
Array.length arr;
Array.sub(arr,0);
