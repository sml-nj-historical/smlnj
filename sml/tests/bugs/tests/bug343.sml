(* bug 343 *)
exception e;
val e' = e;
exception e'' = e'
