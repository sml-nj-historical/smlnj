(* bug12.7.sml *)

signature SS = sig type r val x : r end
(* bug12.1.sml *)

functor F(type t val y: t) : SS =
struct
  datatype r = C of t
  val x = C y
end;

structure S = F(type t = int val y = 3);

S.x;




