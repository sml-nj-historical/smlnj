(* bug614.sml *)
(* 614. high-order-functor thinning-in *)

signature SIG = sig val e : real
		end;

funsig PROD (structure M : SIG
	     structure N : SIG) = SIG;

structure S = struct val e = 100.0
	      end;

structure P = struct val e = 0.0
	      end;

functor Square(structure X : SIG
	       functor Prod : PROD) : SIG =
    Prod(structure M = S
	 structure N = X);

functor F(structure N : SIG) = struct val e = (N.e * N.e)
			       end;

structure A = Square(structure X = P
		     functor Prod = F);

A.e;
