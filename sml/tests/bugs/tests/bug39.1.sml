(* bug39.sml *)

type 'a church = ('a -> 'a) -> ('a -> 'a);
val zero = fn f => fn x => x;
val succ = fn n => fn f => fn x => f (n f x);
val pred = fn n : 'a church =>
	     ((fn (_,b)=>b) (n (fn (a,b) => (succ a, a)) (zero,zero)));
