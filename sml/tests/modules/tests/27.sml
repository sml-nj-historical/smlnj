(* test27.sml *)
(* keywords: functor, open *)

functor F1(X: sig type t val x : t end) =
struct
  structure A = X
  open A
end;

structure S1 = F1(struct type t = int val x = 3 end);
