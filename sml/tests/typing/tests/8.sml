(* 8.sml *)
(* test unification of WILDCARDty from type error against OPEN/FLEX tyvar *)

let fun g {a,...} = 3 in g (f 1.0) end;