(* bug1285.sml *)
(* also modules test 270 *)

signature POINT =
sig 
  type point
end;

signature INTERVAL =
sig
  type point
  val mk : point * point -> unit
end;

functor Interval(P: POINT) : INTERVAL =
struct
  type point = P.point
  fun mk(x,y) = ()  (* <=== culprit is the pair (x,y) *)
end;

structure IntPoint : POINT =
struct
  type point = int
end;

functor G(functor Interv(P: POINT) : INTERVAL) =
struct
  structure NatNumInt = Interv(IntPoint)
end;
