(* 26.sml *)
(* keywords: functor, let *)

(* LETstr form of functor body *)

(* body of LETstr is STRUCTstr *)

functor F1(X: sig type t val x : t end) =
let structure A =
    struct
      type s = X.t list
      val y = X.x :: nil
    end
 in struct
      type u = A.s * X.t
      val z1 = (fn (a:X.t) => ()) (hd A.y)
      val z2 = (fn (b:A.s) => ()) (X.x :: nil)
    end
end;
  
structure B = F1(struct type t = int val x = 3 end);
