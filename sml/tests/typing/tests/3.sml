(* 3.sml *)

fun h a =
    let fun f r x y z u v w = r w
     in (let in f ref end) 1 2 3 4 5 a
    end;

fun f x =
    (fn () => ref x) ();

fun f x =
    let val g = (fn () => ref x)
     in g ()
    end;
