(*
 * Sparc specific backend
 *)
structure SparcCG = 
  MachineGen
  ( structure MachSpec   = SparcSpec
    structure CB	 = CellsBasis
    structure ClientPseudoOps = SparcClientPseudoOps
    structure PseudoOps  = SparcPseudoOps
    structure Ext        = Sparc_SMLNJMLTreeExt(* sparc specific *)
    structure CpsRegs    = SparcCpsRegs
    structure InsnProps  = SparcProps
    structure Asm        = SparcAsmEmitter
    structure Shuffle    = SparcShuffle

    structure CCalls     =
      Sparc_CCalls (structure T = SparcMLTree  fun ix x = x)

    structure OmitFramePtr = struct
      structure CFG=SparcCFG
      structure I=SparcInstr
      val vfp = CpsRegs.vfp
      (* no rewriting necessary, backend uses %fp instead of %sp *)
      fun omitframeptr _ = ()
    end

    structure MLTreeComp=
       Sparc(structure SparcInstr = SparcInstr
             structure SparcMLTree = SparcMLTree
             structure PseudoInstrs = SparcPseudoInstrs
             structure ExtensionComp = SparcMLTreeExtComp
               (structure I = SparcInstr
                structure T = SparcMLTree
                structure Stream = SparcMLTreeStream
		structure CFG = SparcCFG
               )
             val V9 = false
             val muluCost = ref 5
             val multCost = ref 3
             val divuCost = ref 5
             val divtCost = ref 5
             val registerwindow = ref false
             val useBR = ref false
            )

    structure Jumps =
       SparcJumps(structure Instr=SparcInstr
		  structure MLTreeEval=SparcMLTreeEval
                  structure Shuffle=SparcShuffle)

    structure BackPatch =
       SpanDependencyResolution
         (structure CFG	      = SparcCFG
	  structure Placement = DefaultBlockPlacement(SparcCFG) 
          structure Jumps     = Jumps
          structure Emitter   = SparcMCEmitter
          structure DelaySlot = SparcDelaySlots
             (structure I=SparcInstr
              structure P=InsnProps)
          structure Props = InsnProps
         )

    structure RA = 
       RISC_RA
         (structure I         = SparcInstr
          structure Flowgraph = SparcCFG
          structure InsnProps = InsnProps 
          structure Rewrite   = SparcRewrite(SparcInstr)
          structure Asm       = SparcAsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = SparcAsmEmitter)

          structure SpillTable = SpillTable(SparcSpec)
          val fp = I.C.frameptrR
          val spill = CPSRegions.spill
          val beginRA = SpillTable.spillInit
          val architecture = SparcSpec.architecture
         
          fun pure(I.ANNOTATION{i,...}) = pure i
            | pure(I.INSTR(I.LOAD _)) = true
            | pure(I.INSTR(I.FLOAD _)) = true
            | pure(I.INSTR(I.SETHI _)) = true
            | pure(I.INSTR(I.SHIFT _)) = true
            | pure(I.INSTR(I.FPop1 _)) = true
            | pure(I.INSTR(I.FPop2 _)) = true
            | pure _ = false

          (* make copy *) 
          structure Int = 
          struct
             val avail     = SparcCpsRegs.availR
             val dedicated = SparcCpsRegs.dedicatedR

			     
	     fun copy((rds, rss), I.COPY{k=CB.GP, sz, tmp, ...}) = let
		val tmp = (case (rds, rss) of ([_], [_]) => NONE | _ => tmp)
             in I.COPY{k=CB.GP, sz=sz, dst=rds, src=rss, tmp=tmp}
             end

             (* spill copy temp *)
              fun spillCopyTmp(an, I.COPY{k=CB.GP, sz, tmp,dst,src, ...},loc) =
                  I.COPY{k=CB.GP, sz=sz,  dst=dst,src=src,
			 tmp=SOME(I.Displace{base=fp, 
                                             disp=SpillTable.getRegLoc loc})}


             (* spill register *)
             fun spillInstr{src,spilledCell,an,spillLoc} =
                 [I.store{s=I.ST,r=fp,
                          i=I.IMMED(SpillTable.getRegLoc spillLoc), 
                          d=src, mem=spill}]

             (* reload register *)
             fun reloadInstr{dst,spilledCell,an,spillLoc} =
                 [I.load{l=I.LD, r=fp, 
                         i=I.IMMED(SpillTable.getRegLoc spillLoc), 
                         d=dst, mem=spill}
                 ]

             val mode = RACore.NO_OPTIMIZATION
          end

          structure Float = 
          struct
             val avail     = SparcCpsRegs.availF
             val dedicated = SparcCpsRegs.dedicatedF

	      fun copy((fds, fss), I.COPY{k=CB.FP, sz, tmp, ...}) = let
		val tmp =(case (fds, fss) of ([_],[_]) => NONE | _ => tmp)
              in I.COPY{k=CB.FP, sz=sz, dst=fds, src=fss, tmp=tmp}
              end

              fun spillCopyTmp(an, I.COPY{k=CB.FP, sz, tmp,dst,src},loc) =
                  I.COPY{k=CB.FP, sz=sz, dst=dst,src=src,
			 tmp=SOME(I.Displace{base=fp, disp=SpillTable.getFregLoc loc})}
                          

             fun spillInstr(_, d,loc) =
                 [I.fstore{s=I.STDF, r=fp,
                           i=I.IMMED(SpillTable.getFregLoc loc),
                           d=d, mem=spill}]
   
             fun reloadInstr(_, d,loc) =
                 [I.fload{l=I.LDDF, r=fp, 
                          i=I.IMMED(SpillTable.getFregLoc loc),
                          d=d, mem=spill}
                 ]

             val mode = RACore.NO_OPTIMIZATION
          end
         )
  )
