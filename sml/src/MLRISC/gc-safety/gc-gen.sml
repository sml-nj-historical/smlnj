(*
 * This module is reponsible for generating garbage collection 
 * code for all gc-points in the program.  That is, we delay the generation
 * of garbage collection code until all optimizations have been performed.
 * The gc code to be generated is determined by a callback to the client.
 *)

functor GCGen
   (structure MLTreeComp : MLTREECOMP
    structure IR         : MLRISC_IR
    structure GCMap      : GC_MAP
    structure InsnProps  : INSN_PROPERTIES
       sharing MLTreeComp.T.Constant = IR.I.Constant
       sharing MLTreeComp.T.PseudoOp = IR.CFG.P
       sharing IR.I = InsnProps.I = MLTreeComp.I
   ) : GC_GEN =
struct

   structure C   = IR.I.C
   structure T   = MLTreeComp.T
   structure IR  = IR
   structure CFG = IR.CFG
   structure GC  = GCMap.GC
   structure G   = Graph
   structure A   = Array
   structure Liveness = 
      GCLiveness(structure IR = IR
                 structure GCMap = GCMap
                 structure InsnProps = InsnProps)

   structure Gen = CFGGen
      (structure CFG       = CFG
       structure MLTree    = T
       structure InsnProps = InsnProps
      )

   type callgcCallback =
        { id          : int,
          gcLabel     : Label.label,
          returnLabel : Label.label,
          roots       : (C.cell * GC.gctype) list,
          stream      : (T.stm,C.regmap) T.stream
        } -> unit

   fun gcGen {callgc} (IR as G.GRAPH cfg) =
   let (*
        * Run gc-typed liveness analysis
        *)
       val table = Liveness.liveness IR
       val instrStream = Gen.newStream{compile=fn _ => (), flowgraph=SOME IR}
       val stream as T.Stream.STREAM{beginCluster, endCluster, ...} = 
           MLTreeComp.selectInstructions instrStream
 
       (*
        * For each gc-point, invoke the callback to generate GC code.
        *)
       fun process(b,b' as CFG.BLOCK{annotations,insns,...}) =
           case #get MLRiscAnnotations.CALLGC (!annotations) of
             NONE => ()
           | SOME _ =>
           let val {liveIn,liveOut} = A.sub(table,b)
               val roots = liveIn
               val return = #node_info cfg (hd(#succ cfg b))
           in  CFG.changed IR;
               callgc{ id          = b,
                       gcLabel     = CFG.defineLabel b',
                       returnLabel = CFG.defineLabel return,
                       roots       = liveIn,
                       stream      = stream
                     }
           end
           
   in  beginCluster 0;
       #forall_nodes cfg process;
       endCluster [];
       IR
   end

end
