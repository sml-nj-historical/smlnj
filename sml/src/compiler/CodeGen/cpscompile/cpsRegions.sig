signature CPS_REGION = 
sig
  structure PT : POINTS_TO = PointsTo

  type region = PT.loc

  val stack     : region
  val spill     : region
  val readonly  : region
  val memory    : region
  val storelist : region
  val real      : region
  val toString  : region -> string
end
