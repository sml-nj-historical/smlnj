signature S1 = 
    sig
	type t
    end;

signature S2 =
    sig
	type u
	structure A : S1

	sharing type u = A.t
    end;

functor F (X:S2) = struct end;

