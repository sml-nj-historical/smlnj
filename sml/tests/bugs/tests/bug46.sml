(* bug46.sml *)
(*
    when flexrecords are used a nonequality type may be accepted in a context
    where an equality record type is required
*)

fun f(r as {a,...},true) = (r = r)  (* checks only that a admits equality *)
  | f({b,...},false) = b 3; (* oops, the b field is a function! *)
