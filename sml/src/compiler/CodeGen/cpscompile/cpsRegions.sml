structure CPSRegions : CPS_REGION = 
struct
  structure PT = PointsTo

  type region = PT.region

  val memoryCell    = PT.TOP{id=128, name="rw", mutable=true}
  val readonlyCell  = PT.TOP{id=129, name="ro", mutable=false}
  val stackCell     = PT.TOP{id=130, name="stack", mutable=true}
  val spillCell     = PT.TOP{id=131, name="spill", mutable=true}
  val realCell      = PT.TOP{id=132, name="real", mutable=false}
  val storelistCell = PT.TOP{id=133, name="storelist", mutable=true}

  val memory     = ref memoryCell
  val readonly   = ref readonlyCell
  val stack      = ref stackCell
  val spill      = ref spillCell
  val real       = ref realCell
  val storelist  = ref storelistCell

  fun reset() =
      (memory    := memoryCell;
       readonly  := readonlyCell;
       stack     := stackCell;
       spill     := spillCell;
       real      := realCell;
       storelist := storelistCell
      )

  val toString   = PT.toString

end
