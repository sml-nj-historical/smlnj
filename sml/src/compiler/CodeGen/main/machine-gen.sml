(*
 * This is a generic functor that hooks everything together 
 * into an MLRISC backend.
 *)

functor MachineGen
  (structure MachSpec   : MACH_SPEC            (* machine specifications *) 
   structure Ext        : SMLNJ_MLTREE_EXT
   structure InsnProps  : INSN_PROPERTIES      (* instruction properties *)
   structure CpsRegs    : CPSREGS              (* CPS registers *)
		      where T.Region=CPSRegions
		        and T.Constant=SMLNJConstant 
			and T.Extension=Ext
   structure ClientPseudoOps : SMLNJ_PSEUDO_OPS
   structure PseudoOps  : PSEUDO_OPS     (* pseudo ops *)
	 	      where T = CpsRegs.T
		        and Client = ClientPseudoOps
   structure MLTreeComp : MLTREECOMP           (* instruction selection *)
		      where I = InsnProps.I
			and TS.T = CpsRegs.T
		        and TS.S.P = PseudoOps
   structure Asm        : INSTRUCTION_EMITTER  (* assembly *)
		      where S.P = PseudoOps
			and I = MLTreeComp.I
   structure Shuffle    : SHUFFLE              (* shuffling copies *) 
		      where I = Asm.I
   structure BackPatch  : BBSCHED              (* machine code emitter *)
		      where CFG = MLTreeComp.CFG
   structure RA         : CFG_OPTIMIZATION     (* register allocator *)
		      where CFG = BackPatch.CFG
   structure CCalls     : C_CALLS	       (* native C call generator *)
		      where T = CpsRegs.T
   structure OmitFramePtr : OMIT_FRAME_POINTER 
		      where CFG=RA.CFG
  ) : MACHINE_GEN =
struct

   structure G		= Graph 
   structure CFG        = BackPatch.CFG
   structure P          = InsnProps
   structure I          = CFG.I
   structure Cells      = I.C 
   structure T          = MLTreeComp.TS.T
   structure Stream     = MLTreeComp.TS
   structure Asm        = Asm
   structure Shuffle    = Shuffle
   structure MachSpec   = MachSpec
   structure MLTreeComp = MLTreeComp

   (* expand copies into their primitive moves.
    * Copies are no longer treated as span dependent, which was a hack.
    *)
   structure ExpandCpys = 
      CFGExpandCopies
	  (structure CFG = CFG   
	   structure Shuffle = Shuffle)

   structure ComputeFreqs = 
      ComputeFreqsFn(structure CFG=CFG)

   structure BlockPlacement = 
      BlockPlacement
          (structure CFG = CFG 
	   structure Props = InsnProps)

   structure JumpChaining = 
      JumpChainElimFn			     
	  (structure CFG = CFG
	   structure InsnProps = InsnProps)

   structure InvokeGC =
      InvokeGC
	  (structure C     = CpsRegs
	   structure MS    = MachSpec
	   structure CFG   = CFG
	   structure TS    = MLTreeComp.TS
	  )

   fun omitFramePointer(cfg as G.GRAPH graph) = let
     val CFG.INFO{annotations, ...} = #graph_info graph 
   in
     if #contains MLRiscAnnotations.USES_VIRTUAL_FRAME_POINTER (!annotations) then 
     	(OmitFramePtr.omitframeptr
	     {vfp=CpsRegs.vfp, cfg=cfg, idelta=SOME 0:Int32.int option};
	 cfg)
     else cfg
   end     

   fun computeFreqs cfg = (ComputeFreqs.compute cfg;   cfg)

   type mlriscPhase = string * (CFG.cfg -> CFG.cfg) 

   fun phase x = Stats.doPhase (Stats.makePhase x)
   fun makePhase(name,f) = (name, phase name f)

   val mc         = phase "MLRISC BackPatch.bbsched" BackPatch.bbsched
   val placement  = phase "MLRISC Block placement" BlockPlacement.blockPlacement
   val chainJumps = phase "MLRISC Jump chaining" JumpChaining.run
   val finish     = phase "MLRISC BackPatch.finish" BackPatch.finish 
   val compFreqs  = phase "MLRISC Compute frequencies" computeFreqs
   val ra         = phase "MLRISC ra" RA.run
   val omitfp     = phase "MLRISC omit frame pointer" omitFramePointer
   val expandCpys = phase "MLRISC expand copies" ExpandCpys.run
   
   val raPhase = ("ra",ra)

   val optimizerHook = 
     ref [("compFreqs", compFreqs),
	  ("ra", ra),
	  ("omitfp", omitfp),
	  ("expand copies", expandCpys)
	 ]

   fun compile cluster = let
       fun runPhases([],cluster) = cluster
         | runPhases((_,f)::phases,cluster) = runPhases(phases,f cluster)

       fun dumpBlocks cfg = mc (chainJumps (placement cfg))
   in  
       dumpBlocks (runPhases(!optimizerHook,cluster))
   end
 
   (* compilation of CPS to MLRISC *)
   structure MLTreeGen =
      MLRiscGen
	  (structure MachineSpec=MachSpec
           structure MLTreeComp=MLTreeComp
	   structure Ext = Ext
           structure C=CpsRegs
	   structure ClientPseudoOps =ClientPseudoOps
           structure PseudoOp=PseudoOps
           structure InvokeGC=InvokeGC
           structure Flowgen=
	      BuildFlowgraph
                  (structure CFG = CFG
		   structure Props = InsnProps
		   structure Stream = MLTreeComp.TS.S
		  )
	   structure CCalls = CCalls
	   structure Cells = Cells
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
