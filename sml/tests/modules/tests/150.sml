functor F(X : sig type t end) =
struct
  type s = X.t
end;

structure A = F (struct type t = int end);

