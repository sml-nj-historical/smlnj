(* 270.sml *)

signature POINT =
sig 
  type point
  val <= : point * point -> bool
end;

signature INTERVAL =
sig
  type interval
  type point
  val mk : point * point -> interval
  val left : interval -> point
  val right: interval -> point
end;

functor Interval(P: POINT) : INTERVAL =
struct
  type interval = P.point * P.point
  type point = P.point
  fun mk(x,y) = if P.<=(x,y) then (x,y) else (y,x)
  fun left(x,_) = x
  fun right(_,y)= y
end;

structure IntPoint : POINT =
struct
  type point = int
  fun op <= (x, y) = Int.<=(x,y)
end;

structure T = Interval(IntPoint);

val test = T.right(T.mk(3,4))+5;

functor G(functor Interv(P: POINT) : INTERVAL) =
struct
  structure NatNumInt = Interv(IntPoint)
end;
