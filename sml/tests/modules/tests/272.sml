(* 272.sml *)

signature POINT =
sig 
  type point
end

signature INTERVAL =
sig
  type point
  val mk : point -> unit
end

structure IntPoint : POINT =
struct
  type point = int
end

functor H(functor Interv(P: POINT) : 
		sig type point=P.point
		    val mk : point -> unit
		end) =
Interv(IntPoint)

functor RealInterval(P: POINT) : INTERVAL =
struct
  type point = real
  fun mk x = ()
end

structure TH = H(functor Interv = RealInterval)
val _ = TH.mk(3.0)

