(*
 * This is a generic functor that hooks everything together 
 * into an MLRISC backend.
 *)

functor MachineGen
  (structure MachSpec   : MACH_SPEC            (* machine specifications *) 
   structure PseudoOps  : SMLNJ_PSEUDO_OP_TYPE (* pseudo ops *)
   structure CpsRegs    : CPSREGS              (* CPS registers *)
      where T.Region=CPSRegions
        and T.Constant=SMLNJConstant 
        and T.PseudoOp=PseudoOps
        and T.Extension=SMLNJMLTreeExt
   structure InsnProps  : INSN_PROPERTIES      (* instruction properties *)
      where I.Constant = CpsRegs.T.Constant
   structure MLTreeComp : MLTREECOMP           (* instruction selection *)
      where T = CpsRegs.T
        and I = InsnProps.I
   structure Asm        : INSTRUCTION_EMITTER  (* assembly *)
      where S = MLTreeComp.T.Stream
        and P = PseudoOps
        and I = MLTreeComp.I
   structure Shuffle    : SHUFFLE              (* shuffling copies *) 
      where I = MLTreeComp.I
   structure BackPatch  : BBSCHED              (* machine code emitter *)
      where F.P = PseudoOps
        and F.I = Asm.I
   structure RA         : CLUSTER_OPTIMIZATION  (* register allocator *)
      where F = BackPatch.F
  ) : MACHINE_GEN =
struct

   structure F         = BackPatch.F
   structure P         = InsnProps
   structure I         = F.I
   structure Cells     = I.C 
   structure T         = MLTreeComp.T
   structure S         = T.Stream
   structure Asm       = Asm
   structure Shuffle   = Shuffle
   structure MachSpec  = MachSpec
   structure MLTreeComp= MLTreeComp

   type mlriscPhase = string * (F.cluster -> F.cluster) 

   fun phase x = Stats.doPhase (Stats.makePhase x)
   fun makePhase(name,f) = (name, phase name f)

   val mc      = phase "MLRISC BackPatch.bbsched" BackPatch.bbsched
   val finish  = phase "MLRISC BackPatch.finish" BackPatch.finish
   val ra      = phase "MLRISC ra" RA.run

   val raPhase = ("ra",ra)
   val optimizerHook = ref [raPhase]

   (* Flowgraph generation *)
   structure FlowGraphGen =
       ClusterGen(structure Flowgraph = F
                  structure InsnProps = InsnProps
                  structure MLTree    = T
                 )

   (* GC Invocation *)
   structure InvokeGC =
      InvokeGC(structure Cells = Cells
               structure C     = CpsRegs
               structure MS    = MachSpec
              )

   fun compile cluster = 
   let fun runPhases([],cluster) = cluster
         | runPhases((_,f)::phases,cluster) = runPhases(phases,f cluster)
   in  mc(runPhases(!optimizerHook,cluster))
   end 
 
   (* compilation of CPS to MLRISC *)
   structure MLTreeGen =
      MLRiscGen(structure MachineSpec=MachSpec
                structure MLTreeComp=MLTreeComp
                structure Cells=Cells
                structure C=CpsRegs
                structure InvokeGC=InvokeGC
                structure PseudoOp=PseudoOps
                structure Flowgen=FlowGraphGen
                val compile = compile
               )

   val gen = phase "MLRISC MLTreeGen.codegen" MLTreeGen.codegen

   fun codegen x = 
       (* initialize all hidden states first *)
       (Label.reset();
        InvokeGC.init();   
        BackPatch.cleanUp(); 
        gen x
       )

end
