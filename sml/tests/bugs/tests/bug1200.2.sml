(* bug1200.2.sml *)

functor F(structure X : sig type t end
          structure Y : sig type t = X.t end) =
struct
  fun f(x: X.t, y: Y.t list) = x::y
end;

