(* bug232.sml *)

fun f l = let exception E of '_a in (raise (E l)) handle E t => t end;
