signature S6 = sig
    type t

    structure A : sig type t end
end;

functor F6(X:S6) = struct end;

