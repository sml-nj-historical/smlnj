signature S1 =
    sig
	type t
	datatype d = D of t
	val x : d
    end

signature S2 =
    sig
	type u
    end

signature S4 =
    sig
	structure FOO : S1
	structure BAR : S2

	sharing type BAR.u = FOO.t
    end
	    

signature S3 =
    sig
	structure A : 
	    sig
		structure A1 :
		    sig
			structure A2 :
			    sig
				structure FOO : S2
			    end
		    end
	    end
	structure BAR : S1
	structure B : S4
	sharing A.A1.A2.FOO = BAR
	(* sharing type A.A1.A2.FOO.d = B.FOO.d *)
    end
functor F(X:S1) = struct end;
