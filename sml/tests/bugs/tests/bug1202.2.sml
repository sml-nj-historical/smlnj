(* bug1202.2.sml *)

signature SA =
sig
  type t
end;

signature SX =
sig
  structure A : SA
end;

signature SY =
sig
  structure A : SX
end;

functor F
  (structure X : SX
   structure Y : SY  where A = X) =
struct
  fun f(x: X.A.t, y: Y.A.A.t list) = x::y
end;
