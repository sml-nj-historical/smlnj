(* bug118.sml *)

fun divmod (m,n) = (m div n,m mod n);
(* should give (1,2)   *) divmod(5,3);   (* gives (1,2)   *)
(* should give (~2,1)  *) divmod(~5,3);  (* gives (~1,~2) *)
(* should give (~2,~1) *) divmod(5,~3);  (* gives (~1,2)  *)
(* should give (1,~2)  *) divmod(~5,~3); (* gives (1,~2)  *)
