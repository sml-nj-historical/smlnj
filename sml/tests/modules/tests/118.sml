signature S1 =
    sig
	datatype d = D 
    end;

signature S2 =
    sig
	structure A :
	    sig
		structure B : S1
	    end
	structure C : S1

	sharing type A.B.d = C.d
    end;

functor F(X:S2) = struct end;
