structure CPSRegions : CPS_REGION = 
struct
  structure PT = PointsTo

  type region = PT.loc

  val memory     = ref(PT.NAMED("rw",ref(PT.TOP 128)))
  val readonly   = ref(PT.NAMED("ro",ref(PT.TOP 129)))
  val stack      = ref(PT.NAMED("stack",ref(PT.TOP 130)))
  val spill      = ref(PT.NAMED("spill",ref(PT.TOP 131)))
  val real       = ref(PT.NAMED("real",ref(PT.TOP 132)))
  val storelist  = ref(PT.NAMED("storelist",ref(PT.TOP 133)))

  val toString   = PT.toString

end
