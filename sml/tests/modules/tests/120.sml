signature S = sig type t end

signature P = 
    sig
	structure A : S
	structure B : S
	    
	sharing A = B
(*	sharing type A.t = int *)
    end

functor F(X:P) = struct end
