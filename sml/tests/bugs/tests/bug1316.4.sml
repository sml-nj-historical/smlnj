(* bug1316.4.sml *)

fun addList x nil = x      (* programmer bug, should be '= nil' *)
  | addList x L = (x+hd(L))::(addList x (tl L));
