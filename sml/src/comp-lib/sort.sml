(* Copyright 1989 by AT&T Bell Laboratories *)
signature SORT =
  sig
    (* pass the gt predicate as an argument *)
     val sort : ('a * 'a -> bool) -> 'a list -> 'a list  
     val sorted : ('a * 'a -> bool) -> 'a list -> bool  
  end

structure Sort : SORT = struct

(* Industrial-strength quicksort.
   Selects pivot from middle of input list.
   Distributes elements equal to pivot "randomly" in the two output partitions.
   Special-cases lists of 0, 1, or 2 elements.
*)
fun sort (op > : ('x * 'x -> bool)) =
  let fun splita(pivot,nil,less,greater)= qsort less @ (pivot :: qsort greater)
        | splita(pivot,a::rest,less,greater) =
	             if a>pivot then splitb(pivot,rest,less,a::greater)
			        else splitb(pivot,rest,a::less,greater)
      and splitb(pivot,nil,less,greater)= qsort less @ (pivot :: qsort greater)
        | splitb(pivot,a::rest,less,greater) =
	             if pivot>a then splita(pivot,rest,a::less,greater)
			        else splita(pivot,rest,less,a::greater)
      and split1a(pivot,0,_::r,less,greater) = splitb(pivot,r,less,greater)
        | split1a(pivot,i,a::rest,less,greater) =
	             if a>pivot then split1b(pivot,i-1,rest,less,a::greater)
			        else split1b(pivot,i-1,rest,a::less,greater)
      and split1b(pivot,0,_::r,less,greater) = splita(pivot,r,less,greater)
        | split1b(pivot,i,a::rest,less,greater) =
	             if pivot>a then split1a(pivot,i-1,rest,a::less,greater)
			        else split1a(pivot,i-1,rest,less,a::greater)
      and qsort (l as [a,b]) = if a>b then [b,a] else l
        | qsort (l as _::_::_) = 
           let fun getpivot (x::xr, _::_::rest, i) = getpivot(xr,rest,i+1)
                 | getpivot (x::_, _,i) = split1a(x,i,l,nil,nil)
            in getpivot(l,l,0)
           end
        | qsort l = l
  in qsort
  end

(* smooth applicative merge sort
 * Taken from, ML for the Working Programmer, LCPaulson. pg 99-100
 *)
fun sort (op > : 'a * 'a -> bool) ls = 
    let fun merge([],ys) = ys
	  | merge(xs,[]) = xs
	  | merge(x::xs,y::ys) = 
	    if x > y then y::merge(x::xs,ys) else x::merge(xs,y::ys)
	fun mergepairs(ls as [l], k) = ls
	  | mergepairs(l1::l2::ls,k) = 
	    if k mod 2 = 1 then l1::l2::ls
	    else mergepairs(merge(l1,l2)::ls, k div 2)
	fun nextrun(run,[])    = (rev run,[])
	  | nextrun(run,x::xs) = if x > hd run then nextrun(x::run,xs)
				 else (rev run,x::xs)
	fun samsorting([], ls, k)    = hd(mergepairs(ls,0))
	  | samsorting(x::xs, ls, k) = 
	    let val (run,tail) = nextrun([x],xs)
	    in samsorting(tail, mergepairs(run::ls,k+1), k+1)
	    end
    in case ls of [] => [] | _ => samsorting(ls, [], 0)
    end

fun sorted (op >) =
  let fun s (x::(rest as (y::_))) = not(x>y) andalso s rest
        | s l = true
  in s
  end

end

(*
 * $Log$
 *)
