(* test23.sml *)
(* keywords: functor, let *)

(* LETstr form of functor body *)

signature SIG1 =
sig
  type s
  val y : s
end;

functor F3(X: sig type t val x : t end) : SIG1 =
let structure A =
    struct
      type s = X.t list
      val y = X.x :: nil
    end
 in A
end;
  
structure B3 = F3(struct type t = int val x = 3 end);

val x3 = hd(B3.y) + 2;
