(*
 * Simple module for building the IR etc.  Doesn't do any real optimizations.
 *
 * -- Allen
 *)

functor MLRISCGlue
   (structure Asm : INSTRUCTION_EMITTER
    structure F  : FLOWGRAPH
    structure P  : INSN_PROPERTIES
    structure FreqProps : FREQUENCY_PROPERTIES
       sharing P.I = Asm.I = F.I = FreqProps.I
       sharing F.P = Asm.P 
    val copyProp : F.cluster -> F.cluster
   ) : MLRISC_GLUE =
struct

   structure F = F
   structure I = F.I
   structure B = F.B
 
   val viewer  = MLRISC_Control.getString     "viewer"
   val mlrisc  = MLRISC_Control.getFlag       "mlrisc"
   val phases  = MLRISC_Control.getStringList "mlrisc-phases"
   val view_IR = MLRISC_Control.getFlag       "view-IR"
   val verbose = MLRISC_Control.getFlag       "verbose"

   fun error msg = MLRiscErrorMsg.error("MLRISCGlue",msg)

   structure GraphViewer = GraphViewerFn(AllDisplaysFn(val viewer = viewer))

   structure FormatInsn = FormatInstructionFn(Asm)

   structure CFG = ControlFlowGraphFn
      (structure I = I
       structure B = B
       structure P = F.P
       structure GraphImpl = DirectedGraph
       structure Asm = Asm
       structure Ctrl = MLRISC_Control
      )

   structure CFG2Cluster = CFG2ClusterFn
      (structure CFG = CFG
       structure F   = F
      )

   structure Cluster2CFG = Cluster2CFGFn
      (structure CFG = CFG
       structure F   = F
       structure P   = P
      )
       
   structure Dom = DominatorTreeFn(DirectedGraph)

   structure CDG = ControlDependenceGraphFn
      (structure Dom       = Dom
       structure GraphImpl = DirectedGraph
      )

   structure Loop = LoopStructureFn
      (structure Dom       = Dom
       structure GraphImpl = DirectedGraph
      )

   structure Util = CFGUtilFn
      (structure CFG = CFG
       structure P   = P
      )

   structure IR = MLRISC_IRFn
      (structure CFG         = CFG
       structure CDG         = CDG
       structure Loop        = Loop
       structure GraphViewer = GraphViewer
       structure Util        = Util
       structure Ctrl        = MLRISC_Control
      )

   structure Guess = StaticBranchPredictionFn
                        (structure IR = IR
                         structure Props = P
                         structure FreqProps = FreqProps
                        )
      
   structure Liveness = LivenessAnalysisFn(CFG)

   structure Reshape = ReshapeBranchesFn(structure IR = IR
                                         structure P  = P)

   fun view phase ir = if !view_IR then IR.view phase ir else ()

   val ssaParams = {copyPropagation=false,keepName=true,semiPruned=false} 

   fun optimize cluster =
   let datatype rep = IR of IR.IR
                    | CLUSTER of F.cluster
       fun doPhase "copy-prop" (CLUSTER c) = CLUSTER(copyProp c)
         | doPhase "cluster->cfg" (CLUSTER c) = IR(Cluster2CFG.cluster2cfg c)
         | doPhase "cfg->cluster" (IR cfg) = 
            CLUSTER(CFG2Cluster.cfg2cluster{cfg=cfg,relayout=false})
         | doPhase "guess" (r as IR ir) =
            (Guess.profile {loopMultiplier=10} ir; r)
         | doPhase "reshape"   (r as IR ir) = (Reshape.reshapeBranches ir; r)
         | doPhase "view-cfg"  (r as IR ir) = (view "cfg" ir; r)
         | doPhase "view-dom"  (r as IR ir) = (view "dom" ir; r)
         | doPhase "view-pdom" (r as IR ir) = (view "pdom" ir; r)
         | doPhase "view-doms" (r as IR ir) = (view "doms" ir; r)
         | doPhase "view-cdg"  (r as IR ir) = (view "cdg" ir; r)
         | doPhase "view-loop" (r as IR ir) = (view "loop" ir; r)
         | doPhase phase _ = error(phase)
       fun doPhases [] (CLUSTER c) = c
         | doPhases [] _ = error "cluster needed"
         | doPhases (phase::phases) ir = 
            (if !verbose then print("["^phase^"]\n") else (); 
             doPhases phases (doPhase phase ir))
   in  doPhases (!phases) (CLUSTER cluster)
   end

   fun codegen cluster = if !mlrisc then optimize cluster else cluster

end
