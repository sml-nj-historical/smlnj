(* 65.1.sml *)
(* keywords: sharing, functor *)

(* Doligez *)
(* sharing with a free structure obtained by functor application *)
(* status : should work, does not work in 66, works in dd67 *)

functor F (X : sig end) =
struct
  structure A : sig end = X
end;

structure B = struct type t = int end;

structure C = F (B);

signature S =
sig
  structure D : sig type t end = C.A
end;

functor G (X : S) =
  (* we have X.D = C.A = B *)
struct
  val x : X.D.t = 3
end;
