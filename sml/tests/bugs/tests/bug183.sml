(* bug183.sml *)

exception e;
val s = ref 0;
(s := 1, raise e);
s;
(raise e, s := 2);
s;
