(* test25.sml *)
(* keywords: functor, let *)

(* LETstr form of functor body *)

signature SIG1 =
sig
  type s
  val y : s
end;

functor F5(X: sig type t val x : t end) : SIG1 =
let structure A : SIG1 =
    struct
      type s = X.t list
      val y = X.x :: nil
    end
 in A
end;
  
structure B5 = F5(struct type t = int val x = 3 end);

val x5 = hd(B5.y) + 2;
