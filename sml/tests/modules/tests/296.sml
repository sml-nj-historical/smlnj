signature S =
sig
  type t = int list
  type s
  sharing type t = s
end;

functor F(X: S) = struct end;
