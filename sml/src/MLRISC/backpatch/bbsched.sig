signature BBSCHED = sig
  structure CFG : CONTROL_FLOW_GRAPH

  val bbsched : CFG.cfg -> unit
  val finish : unit -> unit
  val cleanUp : unit -> unit
end
