(* bug12.8.sml *)

functor F(type t val y: t) =
struct
  structure A = struct datatype r = C of t end
  val x = A.C y
end;

structure S = F(type t = int val y = 3);

S.x;

