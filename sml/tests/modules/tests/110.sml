signature S5 =
    sig
	structure A : sig end
        structure B : sig end

	sharing A = B
    end;

functor F5 (X:S5) = struct end;
