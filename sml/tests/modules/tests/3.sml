(* test3.sml *)
(* keywords: functor *)

(* caused compiler bug *)

functor F (X : sig type t end) =
struct
  type s = X.t list
end;

functor G (type u) =
struct
  structure A = F (struct type t = u list end)
end;

structure S = G (struct type u = int end);
