(* 271.sml *)

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

structure IntPoint : POINT =
struct
  type point = int
  fun op <= (x, y) = Int.<=(x,y)
end

functor H(functor Interv(P: POINT) : 
		  sig 
		    type interval
		    type point=P.point
		    val mk : point * point -> interval
		    val left : interval -> point
		    val right: interval -> point
		  end) =
struct
  structure NatNumInt = Interv(IntPoint)
end

functor Interval(P: POINT) : INTERVAL =
struct
  type interval = P.point * P.point
  type point = P.point
  fun mk(x,y) = if P.<=(x,y) then (x,y) else (y,x)
  fun left(x,_) = x
  fun right(_,y)= y
end

structure Result = H(functor Interv = Interval)  (* should work *)
structure HI = Result.NatNumInt
val test' = HI.right(HI.mk(3,4))+5

functor RealInterval(P: POINT) : INTERVAL =
struct
  type point = real
  type interval = point * point
  fun mk(x,y) = if Real.<=(x,y) then (x,y) else (y,x)
  fun left(x,_) = x
  fun right(_,y)= y
end

structure ResultH = H(functor Interv = RealInterval)  (* should fail *)
structure HR = ResultH.NatNumInt
val testH = (HR.right(HR.mk(3.0,4.0)))+5.0

