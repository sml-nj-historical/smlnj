(*
 * This module takes a flowgraph and an assembly emitter module and 
 * ties them together into one.  The output is sent to AsmStream.
 *  --Allen
 *)
functor ClusterEmit
  (structure F : FLOWGRAPH
   structure E : INSTRUCTION_EMITTER
      where I = F.I and P = F.P) : ASSEMBLY_EMITTER = 
struct
  type flowgraph = F.cluster
  fun asmEmit(F.CLUSTER{blocks,regmap,annotations=an,...}) = 
  let val E.S.STREAM{pseudoOp,defineLabel,emit,annotation,...} = 
             E.makeStream []
      val emit = emit(E.I.C.lookup regmap)
      fun emitIt(F.PSEUDO pOp) = pseudoOp pOp
        | emitIt(F.LABEL lab)  = defineLabel lab
        | emitIt(F.BBLOCK{insns, annotations=a, ...}) = 
          (app annotation (!a);
           app emit (rev (!insns))
          )
  in  app annotation (!an);
      app emitIt blocks
  end
end

