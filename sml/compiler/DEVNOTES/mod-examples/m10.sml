(* formal types in externally defined signatures
 *)

functor F(X: sig 
	      structure A : ASIG 
	      structure B : BSIG
	  end)
  = struct end