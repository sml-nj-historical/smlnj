(* bug874.sml *)

val (f,prev) =
    let exception notFound
	fun lookup(a,[]) = raise notFound
	  | lookup (a, (b,ans)::rest) = if a=b then ans else lookup(a,rest)
	val prev = ref []
	fun g  0 = 1
	  | g ~1 = 0
	  | g ~2 = 0
	  | g  n = (lookup(n,!prev)
		      handle notFound =>
			let val soln = g(n-3)+g(n-2)+g(n-1)
			in  prev := (n,soln) :: !prev;
			    soln
			end)
     in (g, prev)
    end;

f 35 handle Overflow => 0;
length(!prev);


