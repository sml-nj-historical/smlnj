signature SIG2 =
    sig
	type t
	structure A : sig end
    end;

functor F2 (X:SIG2) = struct end;
