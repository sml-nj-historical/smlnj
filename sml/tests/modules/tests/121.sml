structure A : sig end = 
    struct type t = int
    end;

signature S =
sig
    structure D : sig type t end
    sharing A = D
end;

functor F(X:S) = struct end;
