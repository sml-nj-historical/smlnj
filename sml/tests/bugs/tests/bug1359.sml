(* bug1359.sml *)

let fun g(n, f) = if f = f then (n, true) else f
 in g(0, (0, true))
end;
