functor F(A : sig end) : 
  sig
    structure S : sig end
  end where S = A =
struct
  structure S = A 
end;

