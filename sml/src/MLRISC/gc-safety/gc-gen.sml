(*
 * This module is reponsible for generating garbage collection 
 * code for all gc-points in the program.  That is, we delay the generation
 * of garbage collection code until all optimizations have been performed.
 * The gc code to be generated is determined by a callback to the client.
 *)

functor GCGen
   (structure MLTreeComp : MLTREECOMP
    structure IR         : MLRISC_IR
    structure GC         : GC_TYPE
    structure InsnProps  : INSN_PROPERTIES
       sharing MLTreeComp.T.Constant = IR.I.Constant
       sharing MLTreeComp.T.PseudoOp = IR.CFG.P
       sharing MLTreeComp.T.BNames   = IR.CFG.B
       sharing IR.I = InsnProps.I = MLTreeComp.I
   ) : GC_GEN =
struct

   structure C   = IR.I.C
   structure T   = MLTreeComp.T
   structure IR  = IR
   structure CFG = IR.CFG
   structure GC  = GC
   structure G   = Graph
   structure A   = Array
   structure Liveness = 
      GCLiveness(structure IR = IR
                 structure GC = GC
                 structure InsnProps = InsnProps)

   structure Gen = InstrGen
      (structure MLTree = T
       structure I = IR.I
      )

   type callgcCallback =
        { id     : int,
          label  : Label.label,
          roots  : (C.cell * GC.gctype) list,
          stream : (T.stm,C.regmap) T.stream
        } -> unit

   val debug = MLRiscControl.getFlag "debug-gc-gen"

   fun gcGen {callgc} (IR as G.GRAPH cfg) =
   let (*
        * Run gc-typed liveness analysis
        *)
       val table = Liveness.liveness IR

       (*
        * Check if 
        *)
       fun isGCPoint [] = false
         | isGCPoint(BasicAnnotations.CALLGC::_) = true
         | isGCPoint(_::an) = isGCPoint an

       (*
        * For each gc-point, invoke the callback to generate GC code.
        *)
       fun process(b,b' as CFG.BLOCK{annotations,insns,...}) =
           if isGCPoint(!annotations) then
              let val stream = MLTreeComp.selectInstructions
                                 (Gen.newStream insns)
                  val {liveIn,liveOut} = A.sub(table,b)
                  val roots = liveIn
              in  if !debug then
                     print("id="^Int.toString b^
                           " roots="^Liveness.GCTypeMap.toString roots^"\n")
                  else ();
                  callgc{id     = b,
                         label  = CFG.defineLabel b',
                         roots  = liveIn,
                         stream = stream}
              end
           else ()
           
       val _ = #forall_nodes cfg process
   in  IR
   end

end
