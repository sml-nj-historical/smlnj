(* 4.sml *)

fun left (x,y) = x;
fun right (x,y) = y;

let val later = 
        (SMLofNJ.Cont.callcc (fn k =>
	(fn x => x,fn f => SMLofNJ.Cont.throw k (f, fn u => ()))))
 in print(left(later)"hello");
    right(later)(fn x => x+2)
end;
