(* bug1246.1.sml *)

val 'a f = fn (x:'a) => let val 'a g = fn (y:'a) => (y,y) in g (x,x) end;

