(* 13.sml *)

fun f(x) = let val 'a id :'a -> 'a = fn z => z in id x: 'b end
and g (y: 'b) = f y : 'b;
