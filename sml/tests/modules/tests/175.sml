functor F(A : sig end) : 
  sig
    structure S : sig end sharing S = A
  end=
struct
  structure S = A 
end

