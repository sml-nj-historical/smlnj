(* 273.sml *)

signature POINT =
sig 
  type point
  val <= : point * point -> bool
end

signature INTERVAL =
sig
  type interval
  type point
  val mk : point * point -> interval
  val left : interval -> point
  val right: interval -> point
end

functor Interval(P: POINT) : INTERVAL =
struct
  type interval = P.point * P.point
  type point = P.point
  fun mk(x,y) = if P.<=(x,y) then (x,y) else (y,x)
  fun left(x,_) = x
  fun right(_,y)= y
end

structure IntPoint : POINT =
struct
  type point = int
  fun op <= (x, y) = Int.<=(x,y)
end

functor G(functor Interv(P: POINT) : INTERVAL) =
struct
  structure NatNumInt = Interv(IntPoint)
end

(* RealInterval is a constant functor *)
functor RealInterval(P: POINT) : INTERVAL = 
struct
  type point = real
  type interval = point * point
  fun mk(x,y) = if Real.<=(x,y) then (x,y) else (y,x)
  fun left(x,_) = x
  fun right(_,y)= y
end

structure ResultG = G(functor Interv = RealInterval)
structure TG = ResultG.NatNumInt
val testG = (TG.right(TG.mk(3.0,4.0)))+5.0


