(* 69.1.sml *)
(* keywords: sharing, structure *)

(* Doligez *)
(* sharing with a structure obtained by thinning in the body of a functor *)
(* status : should fail *)

functor F (X : sig end) =
struct
  structure A = struct type t = int end
  structure B : sig end = A
end;

structure C = struct end;

structure D = F (C);

signature S =
sig
  structure E : sig type t end = D.B
end;

functor G (X : S) =
  (* we have X.E = D.B = D.A *)
struct
  val x : X.E.t = 3
end;
