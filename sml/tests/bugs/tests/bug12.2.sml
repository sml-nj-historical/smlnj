(* bug12.2.sml *)

functor F(type t val y: t) =
struct
  datatype r = C of t
  val x = C y
end;

structure S = F(type t = int val y = 3);

S.x;

