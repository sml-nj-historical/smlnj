(* bug265.sml *)

val (f: 'a -> unit, g: ('a * 'b * ('b->unit) -> 'a)) =
    let exception E of 'a
     in ((fn x => raise E x), (fn (a,x,h) => (h x; a) handle E y => y))
    end;

g(true,7,(fn _ => f 0));
