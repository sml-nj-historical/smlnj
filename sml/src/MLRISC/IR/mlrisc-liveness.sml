(*
 * This module performs liveness analysis.
 * It is implemented by instantiating the data flow analyzer module.
 *
 * -- Allen
 *)

signature LIVENESS_ANALYSIS =
sig
  
   structure CFG : CONTROL_FLOW_GRAPH
   structure I   : INSTRUCTIONS
       sharing CFG.I = I

   val liveness : 
       { cfg     : CFG.cfg,
         liveOut : CFG.block Graph.node -> I.C.cell list,
         defUse  : CFG.block Graph.node -> I.C.cell list * I.C.cell list
       } -> unit

   val getLiveness : CFG.cfg -> Graph.node_id -> 
                           {livein: I.C.cell list, liveout: I.C.cell list}

end

functor LivenessAnalysis(CFG : CONTROL_FLOW_GRAPH) : LIVENESS_ANALYSIS =
struct

   structure CFG = CFG
   structure I   = CFG.I
   structure C   = I.C
   structure A   = Annotations
   structure SL  = SortedList
   structure G   = Graph

   val livenessProp = A.new (SOME(fn _ => "liveness")) : 
          (C.cell list * C.cell list) A.property

   structure Liveness =
      Dataflow
         (struct
              structure CFG   = CFG
              type domain     = C.cell list
              val  forward    = false
              val  bot        = []
              val  ==         = op = : C.cell list * C.cell list -> bool
              val  join       = SL.foldmerge
              val  op +       = SL.merge
              val  op -       = SL.difference
              type dataflow_info = 
                  { liveOut : CFG.block Graph.node -> C.cell list,
                    defUse  : CFG.block Graph.node -> 
                                  C.cell list * C.cell list
                  }

              fun prologue(cfg,{defUse,liveOut}) (b,b') =
                  let val (def,use) = defUse(b,b')
                      val live_out  = liveOut(b,b')
                  in  { input    = live_out,
	                output   = (live_out - def) + use,
	                transfer = fn live_out => (live_out - def) + use
                      }
                  end

              fun epilogue _ { node = (b,CFG.BLOCK{annotations,...}), 
                               input=liveOut, output=liveIn } = 
                  annotations := #set livenessProp 
                                  ((liveIn,liveOut),!annotations)
         end
        )

   fun liveness {cfg,liveOut,defUse} = 
      (Liveness.analyze(cfg,{liveOut=liveOut,defUse=defUse}); ())

   fun getLiveness (G.GRAPH cfg) b = 
       let val CFG.BLOCK{annotations,...} = #node_info cfg b
       in  case #get livenessProp (!annotations) of
              SOME(x,y) => {livein=x,liveout=y}
           |  NONE => {livein=[],liveout=[]}
       end

end

