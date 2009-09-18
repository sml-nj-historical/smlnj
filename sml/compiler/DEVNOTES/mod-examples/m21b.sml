signature S = sig type t end;

functor H(X: S) = 
struct
   functor H1(Y: sig val a : X.t end) = struct end
end;

structure A = H(struct type t = int end);

functor F = A.H1;
