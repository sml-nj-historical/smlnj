(* bug51.sml -- result sig mentions parameter structure *)

functor F(S: sig type t val x: t end) : sig val y : S.t end =
struct
   val y = S.x
end
