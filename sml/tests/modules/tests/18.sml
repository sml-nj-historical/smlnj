(* test18.sml *)
(* keywords: functor *)

(* derived from bug 172 *)

signature SIG =
sig
  type t
  structure A : sig val y : t end
end;

functor F (structure X : SIG) =  (* needs structure keyword *)
struct
  val z = X.A.y
end;
