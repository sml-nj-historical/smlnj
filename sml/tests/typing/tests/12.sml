(* 12.sml *)

fun f(x) = let val 'a id :'a -> 'a = fn z => z in id x: 'a end
and g (y: 'a) = f y : 'a;
