(* bug12.1.sml *)

functor F(type t val y: t) : sig type r val x : r end =
struct
  datatype r = C of t
  val x = C y
end;

structure S = F(type t = int val y = 3);

S.x;

