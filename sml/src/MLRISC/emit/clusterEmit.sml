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
  let val E.S.STREAM{pseudoOp,defineLabel,emit,annotation,comment,...} = 
             E.makeStream (!an)
      val emit = emit(E.I.C.lookup regmap)
      fun emitAn a = if Annotations.toString a = "" then () else annotation(a)
      fun emitIt(F.PSEUDO pOp) = pseudoOp pOp
        | emitIt(F.LABEL lab)  = defineLabel lab
        | emitIt(F.BBLOCK{insns, annotations=a, ...}) = 
          (app emitAn (!a);
           app emit (rev (!insns))
          )
        | emitIt _ = ()
  in  app annotation (!an);
      app emitIt blocks
  end
end

