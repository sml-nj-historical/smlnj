signature S1 = 
    sig
	type t
    end;

signature S2 =
    sig
	type u
    end;


signature S3 =
    sig
	type t
	type u
    end

signature S4 =
    sig
	structure A : S1
	structure B : S2
	structure C : S3

	sharing A = B
	sharing A = C
    end;

functor F (X:S4) = struct end;
