(* test24.sml *)
(* keywords: functor, let *)

(* LETstr form of functor body *)

signature SIG1 =
sig
  type s
  val y : s
end;

functor F4(X: sig type t val x : t end) =
let structure A : SIG1 =
    struct
      type s = X.t list
      val y = X.x :: nil
    end
 in A
end;
  
structure B4 = F4(struct type t = int val x = 3 end);

val x4 = hd(B4.y) + 2;
