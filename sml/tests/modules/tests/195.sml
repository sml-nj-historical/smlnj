signature SIG1 =
sig
  datatype t = T of int
end

signature SIG2 =
sig
  structure N : SIG1
end

functor F(structure X : SIG1
          structure Y : SIG2 where N = X) =
struct end;
