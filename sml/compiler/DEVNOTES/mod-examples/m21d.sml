functor H(X: sig type t end) = 
struct
   functor H1(Y: sig val a : int end) = struct end
end;

structure A = H(struct type t = int end);

structure B = A.H1(struct val a = 3 end);
