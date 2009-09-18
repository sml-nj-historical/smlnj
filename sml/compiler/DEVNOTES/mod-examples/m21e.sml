functor H(X: sig type t end) = 
struct
   functor H1(Y: sig val a : X.t end) = struct end
end;

structure P = struct type t = int end;

structure A = H(P);

structure B = A.H1(struct val a: P.t = 3 end);
