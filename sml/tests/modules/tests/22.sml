(* test22.sml *)
(* keywords: functor, let *)

(* LETstr form of functor body *)

functor F2(X: sig type t val x : t end) : sig type s val y : X.t list end =
let structure A =
    struct
      type s = X.t list
      val y = X.x :: nil
    end
 in A
end;
  
structure B2 = F2(struct type t = int val x = 3 end);

val x2 = hd(B2.y) + 2;
