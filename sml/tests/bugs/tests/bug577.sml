(* bug577.sml *)
(* 577. use of vectors crashes NeXT *)

open Vector;

fun update(array,index,value) = 
    let
	fun copy i = 

	    if i = index then value
	    else sub(array,i)
	val size = length array
    in
	if index < size then
	    tabulate (size,copy)
	else
	    raise Subscript
    end;

fun reverse original =
    let
	val size = length original
	fun rev (current,count) =
	    if count < size then
		let
		    val count'  = count + 1
		    val current' =
			update(current, count, sub(original, size-count'))
		in
		    rev (current', count')
		end
	    else current
    in
	rev (original,0)
    end;

fun bug n = reverse (tabulate (n, fn x => x));

bug 500;

bug 1000;
