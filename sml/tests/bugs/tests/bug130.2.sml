(* bug 130, compiler bug *)

structure S =
struct
  datatype 'a T =  C
  fun fs(x: 'a T) = x
end;

functor F(X: sig type t end) =
struct
  type FT = X.t S.T
  fun ff(x : FT) = S.fs x
end;

structure SF = F(struct type t = int end);
