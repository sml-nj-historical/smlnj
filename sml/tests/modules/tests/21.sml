(* test21.sml *)
(* keywords: functor, let *)

(* LETstr form of functor body *)
(* body of LETstr is VARstr *)

functor F1(X: sig type t val x : t end) =
let structure A =
    struct
      type s = X.t list
      val y = X.x :: nil
    end
 in A
end;
  
structure B1 = F1(struct type t = int val x = 3 end);

val x1 = hd(B1.y) + 2;
