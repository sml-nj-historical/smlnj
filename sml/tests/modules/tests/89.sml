functor F(A:sig type t end) = A

structure S = struct type t=int end

structure T = F(S)
