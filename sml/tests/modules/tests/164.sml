signature S =
sig
  type t
  val x: t
end;

functor F(X: S) = 
struct
  val y = X.x
end;

functor G(Y : S) =
struct
  structure A = F(Y)
  val _ = A.y
end;
