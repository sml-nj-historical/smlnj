(* 55.1.sml *)
(* keywords: functor, sharing *)

(* Doligez *)
(* non-generative functor (F), applied in another functor (G) *)
(* status : should work, does not work in 66, works in dd67 *)

functor F (X : sig end) = X;

functor G (X : sig end) =
  struct structure A : sig end = F (X)
  end;

structure B = struct type t = int end;

structure C = G (B);

signature S =
  sig structure D : sig type t end = C.A
  end;

functor H (X : S) =
  (* we have X.D = C.A = F(B) = B *)
  struct val x : X.D.t = 3
  end;
