(*
 * This module builds a CFG from a stream of instructions.
 * We use the FLOWGRPAH_GEN interface here, which is the 
 * default interface used by the core MLRISC.
 *
 * -- Allen
 *)

functor CFGGen
  (structure CFG : CONTROL_FLOW_GRAPH
   structure InsnProps : INSN_PROPERTIES
   structure MLTree : MLTREE
     sharing CFG.I = InsnProps.I
     sharing MLTree.Constant = InsnProps.I.Constant
     sharing MLTree.PseudoOp = CFG.P 
  ) : FLOWGRAPH_GEN =
struct

   structure I = CFG.I
   structure C = I.C
   structure S = MLTree.Stream
   structure T = MLTree
   structure P = CFG.P
   structure Builder = ControlFlowGraphGenFn
     (structure CFG = CFG
      structure Stream = S
      structure InsnProps = InsnProps
     )

   type flowgraph = CFG.cfg

   fun newStream{compile,flowgraph} =
   let val cfg = ref(case flowgraph of
                       NONE => CFG.new(I.C.regmap())
                     | SOME cfg => cfg
                    )
       val {stream,next} = Builder.builder(!cfg)
       val S.STREAM{beginCluster,endCluster,pseudoOp,emit,exitBlock,
                    comment,annotation,defineLabel,entryLabel,alias,phi,...} 
                      = stream
       fun exit liveRegs = 
       let val addReg   = C.addCell C.GP
           val addFreg  = C.addCell C.FP
           val addCCreg = C.addCell C.CC
           (* we don't care about memory locations that may be live. *)
           fun live(T.GPR(T.REG(_,r))::rest, acc) = live(rest, addReg(r, acc))
             | live(T.FPR(T.FREG(_,f))::rest, acc) = live(rest, addFreg(f, acc))
             | live(T.CCR(T.CC c)::rest, acc) = live(rest, addCCreg(c, acc))
             | live(_::rest, acc) = live(rest, acc)
             | live([], acc) = acc

           val lout = live(liveRegs, C.empty)
       in  exitBlock(lout) end

       fun endCFG a = 
       let val _      = endCluster a
           val oldCFG = !cfg
           val newCFG = CFG.new(I.C.regmap())
       in  cfg := newCFG;
           next newCFG;
           compile oldCFG
       end 

   in  S.STREAM{beginCluster= beginCluster,
                endCluster  = endCFG,
                pseudoOp    = pseudoOp,
                emit        = emit,
                exitBlock   = exit,
                comment     = comment,
                annotation  = annotation,
                defineLabel = defineLabel,
                entryLabel  = entryLabel,
                alias       = alias,
                phi         = phi
               }
   end

end

