(* 69.2.sml *)
(* keywords: sharing, structure *)

(* Doligez *)
(* sharing with a structure obtained by thinning in the body of a functor *)
(* status : should fail *)

functor F (X : sig end) =
struct
  structure A = struct type t = int end
end;

structure C = struct end;

structure D = F (C);

signature S =
sig
  structure E : sig type t end = D.A
end;

functor G (X : S) =
struct
  val x : X.E.t = 3
end;
