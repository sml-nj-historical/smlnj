(* bug1593.1.sml *)

fun f 0w1073741823 = false
  | f _ = true;

(* 0w1073741823 is 2^30-1 *)
