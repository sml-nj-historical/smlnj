(* test56.sml *)
(* keywords: functor, sharing *)

(* Doligez *)

(* generative functor, applied in another functor *)
(* status : should not work, does not work in 66, does not work in dd67 *)

functor F (X : sig end) =
struct
  datatype t = c
  val x = c
end;

functor G (X : sig end) =
struct
  structure A : sig type t val x : t end = F (X)
end;

structure B = struct end;

structure C = G (B);

structure D = G (B);

(* C.A.t is different from D.A.t *)
val x : bool = (C.A.x = D.A.x);
