(* bug815.sml *)
(* 815. incorrect paths printed for types (missing structures) *)

functor F(X: sig functor G() : sig type t end end) =
struct
  structure A = X.G()
end;

structure Rint = F(struct functor G() = struct type t = int end end);

structure Rbool = F(struct functor G() = struct type t = bool end end);

val x : Rint.A.t = 3;
val y : Rbool.A.t = true;
