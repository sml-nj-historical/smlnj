(*
 * This is a generic functor that hooks everything together 
 * into an MLRISC backend.
 *)

functor MachineGen
  (structure MachSpec   : MACH_SPEC            (* machine specifications *) 
   structure PseudoOps  : SMLNJ_PSEUDO_OP_TYPE (* pseudo ops *)
   structure CpsRegs    : CPSREGS              (* CPS registers *)
      where T.Region=CPSRegions
      where T.Constant=SMLNJConstant 
      where T.PseudoOp=PseudoOps
   structure InsnProps  : INSN_PROPERTIES      (* instruction properties *)
      where I.Constant = CpsRegs.T.Constant
   structure MLTreeComp : MLTREECOMP           (* instruction selection *)
      where T = CpsRegs.T
      where I = InsnProps.I
   structure Asm        : INSTRUCTION_EMITTER  (* assembly *)
      where S = MLTreeComp.T.Stream
      where P = PseudoOps
      where I = MLTreeComp.I
   structure BackPatch  : BBSCHED              (* machine code emitter *)
      where F.P = PseudoOps
      where F.I = Asm.I
   structure RA         : REGALLOC             (* register allocator *)
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
   structure MachSpec  = MachSpec
   structure MLTreeComp= MLTreeComp

   val optimizerHook : (F.cluster -> F.cluster) option ref = ref NONE

   fun phase x = Stats.doPhase (Stats.makePhase x)

   fun opt cluster =
       case !optimizerHook of 
         SOME f => f cluster 
       | NONE => cluster
      
   val ra      = phase "MLRISC ra" RA.ra
   val opt     = phase "MLRISC optimization" opt 
   val bbsched = phase "MLRISC BackPatch.bbsched" BackPatch.bbsched
   val finish  = phase "MLRISC BackPatch.finish" BackPatch.finish

   fun compile cluster = 
   let val cluster = opt cluster
       val cluster = ra cluster
   in  bbsched cluster end 
  
   (* Flowgraph generation *)
   structure FlowGraphGen =
       ClusterGen(structure Flowgraph = F
                  structure InsnProps = InsnProps
                  structure MLTree    = T
                  val output = compile
                 )

   (* GC Invocation *)
   structure InvokeGC =
      InvokeGC(structure Cells = Cells
               structure C     = CpsRegs
               structure MS    = MachSpec
              )

   (* compilation of CPS to MLRISC *)
   structure MLTreeGen =
      MLRiscGen(structure MachineSpec=MachSpec
                structure MLTreeComp=MLTreeComp
                structure Cells=Cells
                structure C=CpsRegs
                structure InvokeGC=InvokeGC
                structure PseudoOp=PseudoOps
                structure CpsTreeify=CpsTreeify
                structure Flowgen=FlowGraphGen
               )
   val gen = phase "MLRISC MLTreeGen.codegen" MLTreeGen.codegen

   fun codegen x = 
       (* initialize all hidden states first *)
       (Label.reset();
        InvokeGC.init();   
        BackPatch.cleanUp(); 
        gen x
       )
   val copyProp = RA.cp

end
