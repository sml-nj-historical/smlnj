(* bug212.sml *)
(* 0.56 rejects proper weak polymorphic type for locally declared exception *)

fun f (l:'_a) = let exception E of '_a in (raise (E l)) handle E t => t end;
