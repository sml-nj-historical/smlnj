(* bug95.sml *)

type ('a,'b) --> = 'a -> 'b;
val a = fn _ => 5;
a : ('a,int) -->;
infix -->;
a : ('a,int) -->;
