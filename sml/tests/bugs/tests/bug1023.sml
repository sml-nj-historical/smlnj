(* bug1023.sml *)

fun a2v a = Array.extract(a, 0, NONE);
val a0 : int Array.array = Array.fromList [];
a2v a0;
