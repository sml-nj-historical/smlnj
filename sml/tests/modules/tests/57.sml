(* test57.sml *)
(* keywords: functor, sharing *)

(* Doligez *)

(* type propagated by functor argument *)
(* status : should work, works in 66, works in dd67 *)

functor F (X : sig type t val x : t end) =
  struct type t = X.t
         val x = X.x
  end;

structure B = struct datatype t = c val x = c end;

structure C = F (B);

structure D = F (B);

(* C.t = B.t = D.t *)
val x : bool = (C.x = D.x);
