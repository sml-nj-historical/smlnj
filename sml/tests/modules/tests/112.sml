signature S7 = 
    sig
	structure A : 
	    sig
		structure A1 : sig end
	    end

    end;

functor F7 (X:S7) = struct end;

