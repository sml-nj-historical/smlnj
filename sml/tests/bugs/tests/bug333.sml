(* bug 333 -- causes Regbind to be raised in compiler *)

fun sine x = (if 1 mod 4 = 0 then Math.exp(x) else Math.exp(real 1 * x)) / sine x;
    
