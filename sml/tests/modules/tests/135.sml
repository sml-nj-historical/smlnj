signature S =
sig
  type t = int
  type u = t list
end;

functor F(X: S) =
struct
end;
