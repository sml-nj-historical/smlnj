signature S = sig type t end;

functor F(X : S) =
struct
  type s = X.t
end;

structure R : S = struct type t = int end;

structure A = F(R);


