(* bug1200.1.sml *)

functor F(structure X : sig type t end
          structure Y : sig type t end where type t = X.t) =
struct
  fun f(x: X.t, y: Y.t list) = x::y
end;

