(*
 * SSA optimizer for doing experiments 
 *)

functor SSAOptimizerFn
   (structure Asm : INSTRUCTION_EMITTER
    structure F  : FLOWGRAPH
    structure P  : INSN_PROPERTIES
    structure SP : SSA_PROPERTIES
    structure FreqProps : FREQUENCY_PROPERTIES
       sharing P.I = SP.I = Asm.I = F.I = FreqProps.I
       sharing F.P = Asm.P 
    val copyProp : F.cluster -> F.cluster
   ) : SSA_OPTIMIZER =
struct

   structure F = F
   structure I = F.I
   structure B = F.B
   structure Ctrl = MLRISC_Control

   val view_IR    = Ctrl.getFlag "view-IR" 
   val verbose    = Ctrl.getFlag "verbose"
   val viewer     = Ctrl.getString "viewer"
   val min_blocks = Ctrl.getInt "min-blocks"

   fun error msg = MLRiscErrorMsg.error("SSAOptimizer",msg)

   structure GraphViewer = GraphViewerFn(AllDisplaysFn(val viewer = viewer))

   structure FormatInsn = FormatInstructionFn(Asm)

   structure CFG = ControlFlowGraphFn
      (structure I = I
       structure B = B
       structure P = F.P
       structure GraphImpl = DirectedGraph
       structure Asm = Asm
       structure Ctrl = Ctrl
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
       structure Ctrl        = Ctrl
      )

   structure Guess = StaticBranchPredictionFn(structure IR = IR
                                              structure Props = P
                                              structure FreqProps = FreqProps
                                             )
      
   structure Liveness = LivenessAnalysisFn(CFG)

   structure SSA = SSAFn
      (structure CFG  = CFG 
       structure Dom  = Dom
       structure SP   = SP
       structure P    = P
       structure RTL  = SP.RTL
       structure FormatInsn = FormatInsn
       structure GraphImpl = DirectedGraph
       structure Ctrl = Ctrl
      )
      
   structure CFG2SSA = CFG2SSAFn
      (structure SSA = SSA
       structure Liveness = Liveness
      )

   structure Reshape = ReshapeBranchesFn(structure IR = IR
                                         structure P  = P)

   structure InsertPreheaders = InsertPreheadersFn(structure IR = IR
                                                   structure P  = P
                                                   structure Ctrl = Ctrl)

   structure SSADCE = SSADeadCodeElimFn(SSA)

   structure CF  = SSAConstantFoldingFn(SSA)

   structure GVN = SSAGlobalValueNumberingFn(CF)

   structure SSAGVN = SSAGVNFn(structure GVN = GVN 
                               val leaveBehindCopy = false
                               val foldGlobalConstants = true)

   structure SSAGVNL = SSAGVNFn(structure GVN = GVN 
                               val leaveBehindCopy = false
                               val foldGlobalConstants = false)

   structure SSAGVN' = SSAGVNFn(structure GVN = GVN 
                               val leaveBehindCopy = false
                               val foldGlobalConstants = true)

   structure SSAGCM = SSAGlobalCodeMotionFn(SSA)
   (* structure SSAGCM2 = SSAGlobalCodeMotion2Fn(SSA) *)
   structure Depressurize = SSADepressurizeFn(SSA)

   structure SSALiveness = SSALivenessFn(SSA)

   structure SSA2CFG = SSA2CFGFn
      (structure SSA      = SSA
       structure Liveness = SSALiveness
       structure P        = P
       structure Util     = Util
       structure Ctrl     = Ctrl
      ) 

   fun view phase ir = if !view_IR then IR.view phase ir else ()

   fun optimize cluster =
   let datatype rep = IR of IR.IR
                    | CLUSTER of F.cluster
                    | SSA of SSA.ssa
       fun doPhase "copy-prop" (CLUSTER c) = CLUSTER(copyProp c)
         | doPhase "cluster->cfg" (CLUSTER c) = IR(Cluster2CFG.cluster2cfg c)
         | doPhase "cfg->cluster" (IR cfg) = 
            CLUSTER(CFG2Cluster.cfg2cluster{cfg=cfg,relayout=false})
         | doPhase "guess" (r as IR ir) =
             (Guess.profile {loopMultiplier=10} ir; r)
         | doPhase "reshape"   (r as IR ir) = (Reshape.reshapeBranches ir; r)
         | doPhase "insert-preheaders" (r as IR ir) = 
             (InsertPreheaders.insert_preheaders ir; r)
         | doPhase "split-critical-edges" (r as IR ir) = 
             (Util.splitAllCriticalEdges ir; r)
         | doPhase "view-cfg"  (r as IR ir) = (view "cfg" ir; r)
         | doPhase "view-dom"  (r as IR ir) = (view "dom" ir; r)
         | doPhase "view-doms" (r as IR ir) = (view "doms" ir; r)
         | doPhase "view-cdg"  (r as IR ir) = (view "cdg" ir; r)
         | doPhase "view-loop" (r as IR ir) = (view "loop" ir; r)
         | doPhase "view-ssacfg"  (r as SSA ssa) = 
            (if !view_IR then GraphViewer.view (SSA.viewAsCFG ssa) else (); r)
         | doPhase "view-ssa"  (r as SSA ssa) = 
            (if !view_IR then GraphViewer.view (SSA.viewAsSSA ssa) else (); r)
         | doPhase "cfg->ssa"  (IR ir)   = SSA(CFG2SSA.buildSSA(ir,IR.dom ir))
         | doPhase "ssa-dce"   (SSA ssa) = SSA(SSADCE.optimize ssa)
         | doPhase "ssa-gvn"   (SSA ssa) = SSA(SSAGVN.optimize ssa)
         | doPhase "ssa-gvnl"  (SSA ssa) = SSA(SSAGVNL.optimize ssa)
         | doPhase "ssa-gvn'"  (SSA ssa) = SSA(SSAGVN'.optimize ssa)
         | doPhase "ssa-gcm"   (SSA ssa) = SSA(SSAGCM.optimize ssa)
         (* | doPhase "ssa-gcm2"  (SSA ssa) = SSA(SSAGCM2.optimize ssa) *)
         | doPhase "ssa-dep"   (SSA ssa) = SSA(Depressurize.optimize ssa)
         | doPhase "gvn"       (r as SSA ssa) =
              (GVN.computeValueNumbers ssa; r)
         | doPhase "ssa->cfg"  (SSA ssa) = IR(SSA2CFG.buildCFG ssa)
         | doPhase phase _ = error(phase)
       fun doPhases [] (CLUSTER c) = c
         | doPhases [] _ = error "cluster needed"
         | doPhases (phase::phases) ir = 
            let fun pr msg = TextIO.output(TextIO.stdErr,msg) 
                val _  = if !verbose then pr("[ start "^phase^"]") else (); 
                val timer = Timer.startCPUTimer()
                val ir = doPhase phase ir handle e =>
                     (print("[ "^phase^": uncaught exception: "
                            ^exnName e^" ]\n"); raise e)
                val {gc,sys,usr} = Timer.checkCPUTimer timer
                val _  = if !verbose then 
                         pr("[ end "^phase^" usr="^Time.toString usr^
                            " sys="^Time.toString sys^
                            " gc="^Time.toString gc^"]\n") else ();
            in  doPhases phases ir end
       
       val F.CLUSTER{blocks,...} = cluster
       fun isAllGC([],gc,n) = (gc,n)
         | isAllGC(F.BBLOCK{succ,pred,...}::bs,gc,n) =  
               isAllGC(bs,gc andalso (case (!succ,!pred) of
                                        ([_],[_]) => true | _ => false),n+1)
         | isAllGC(_::bs,gc,n) = isAllGC(bs,gc,n) 
   in  case isAllGC(blocks,true,0) of
         (true,_) => cluster
       | (false,n) =>
         if n >= !min_blocks then
            doPhases (!Ctrl.mlrisc_phases) (CLUSTER cluster)
         else
            cluster
   end

   fun codegen cluster = if !Ctrl.mlrisc then optimize cluster else cluster

end
