functor F() =
struct
  structure S : sig type v end =
    struct
       datatype u = U
       type v = u
    end
  datatype t = T of S.v
end;

structure S = F();
