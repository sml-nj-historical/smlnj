(* bug39a.sml *)

type 'a church = ('a -> 'a) -> ('a -> 'a);
val zero = fn f => fn x => x;
val succ = fn n : 'a church => fn f => fn x => f (n f x);
