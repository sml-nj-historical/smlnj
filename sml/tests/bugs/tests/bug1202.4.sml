(* bug1202.4.sml *)

signature SA =
sig
  type t
end;

signature SX =
sig
  structure A : SA
end;

functor F
  (structure X : SX
   structure Y : sig structure A : SA = X.A end) =
struct
  fun f(x: X.A.t, y: Y.A.t list) = x::y
end;
