functor F(A : sig end) : 
  sig
    structure S : sig end = A
  end =
struct
  structure S = A 
end;

