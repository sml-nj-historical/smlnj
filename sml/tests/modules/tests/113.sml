signature S81 = sig end;
signature S82 =
    sig
	structure A : S81
    end;

functor F (X:S82) = struct end;
