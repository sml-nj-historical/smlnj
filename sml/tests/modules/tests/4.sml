(* test4.sml *)
(* keywords: functor *)

(* caused Subscript exception *)

functor F (X : sig type t end) =
struct
  type s = X.t
end;

functor G (Y : sig type u end) =
struct
  structure A = F (struct type t = Y.u list end)
end;

structure S = G (struct type u = int end);
