(* bug466.sml *)
(* looping error message *)

fun f (p,q) =
    let fun g (p,q) = #1 q orelse f (p,q)
     in g (p, #2 q)
    end;
