(* bug878.sml *)
(* 878. "Error: Compiler bug: getSymbols" compiling h.o. functor *)

signature MONOID =
    sig
	type t
	val plus: t * t -> t
	val e:t
    end;
    

funsig PROD (structure M:MONOID and N:MONOID) = MONOID;

functor Square (structure X:MONOID functor Prod:PROD) =
    Prod(structure M=X and N=X);
    

structure IntMonoid : MONOID =
    struct
	type t = int
	fun plus (x,y) = x+y : int
	val e = 0
    end;
    
    
functor Prod (structure M:MONOID and N:MONOID) : MONOID =
    struct
	type t = M.t * N.t
	fun plus ((m1,n1), (m2,n2) ) = (M.plus (m1,m2), N.plus(n1,n2))
	val e = (M.e,N.e)
    end;
    
structure IntPairMonoid  = Square (structure X = IntMonoid 
                                  functor Prod = Prod);
    
open IntPairMonoid;

(*
No problem if signature added to the above:
structure IntPairMonoid: MONOID  = Square (structure X = IntMonoid 
                                   functor Prod = Prod);
*)
