signature S3 =
    sig
	structure A : sig end
    end;

functor F1 (X : S3) = struct end
