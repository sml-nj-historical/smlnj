(*
 * Sparc specific backend
 *)
structure SparcCG = 
  MachineGen
  ( structure MachSpec   = SparcSpec
    structure PseudoOps  = SparcPseudoOps
    structure Ext        = SMLNJMLTreeExt(* generic extension *)
    structure CpsRegs    = SparcCpsRegs
    structure InsnProps  = SparcProps
    structure Asm        = SparcAsmEmitter
    structure Shuffle    = SparcShuffle

    structure CCalls     = DummyCCallsFn (SparcMLTree)

    structure MLTreeComp=
       Sparc(structure SparcInstr = SparcInstr
             structure SparcMLTree = SparcMLTree
             structure PseudoInstrs = SparcPseudoInstrs
             structure ExtensionComp = SMLNJMLTreeExtComp
               (structure I = SparcInstr
                structure T = SparcMLTree
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
                  structure Shuffle=SparcShuffle)

    structure BackPatch =
       SpanDependencyResolution
         (structure Flowgraph = SparcFlowGraph
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
          structure Flowgraph = SparcFlowGraph
          structure InsnProps = InsnProps 
          structure Rewrite   = SparcRewrite(SparcInstr)
          structure Asm       = SparcAsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = SparcAsmEmitter)

          structure SpillTable = SpillTable(SparcSpec)
          val sp = I.C.stackptrR
          val spill = CPSRegions.spill
          val beginRA = SpillTable.spillInit
          val architecture = SparcSpec.architecture
         
          fun pure(I.ANNOTATION{i,...}) = pure i
            | pure(I.LOAD _) = true
            | pure(I.FLOAD _) = true
            | pure(I.SETHI _) = true
            | pure(I.SHIFT _) = true
            | pure(I.FPop1 _) = true
            | pure(I.FPop2 _) = true
            | pure _ = false

          (* make copy *)
          structure Int = 
          struct
             val avail     = SparcCpsRegs.availR
             val dedicated = SparcCpsRegs.dedicatedR

             fun copy((rds as [_], rss as [_]), _) =
                 I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
               | copy((rds, rss), I.COPY{tmp, ...}) =
                 I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}


             (* spill copy temp *)
             fun spillCopyTmp(_, I.COPY{dst,src,tmp,impl},loc) =
                 I.COPY{dst=dst, src=src, impl=impl,
                        tmp=SOME(I.Displace{base=sp, 
                                            disp=SpillTable.getRegLoc loc})}

             (* spill register *)
             fun spillInstr{src,spilledCell,an,spillLoc} =
                 [I.STORE{s=I.ST,r=sp,
                          i=I.IMMED(SpillTable.getRegLoc spillLoc), 
                          d=src, mem=spill}]

             (* reload register *)
             fun reloadInstr{dst,spilledCell,an,spillLoc} =
                 [I.LOAD{l=I.LD, r=sp, 
                         i=I.IMMED(SpillTable.getRegLoc spillLoc), 
                         d=dst, mem=spill}
                 ]

             val mode = RACore.NO_OPTIMIZATION
          end

          structure Float = 
          struct
             val avail     = SparcCpsRegs.availF
             val dedicated = SparcCpsRegs.dedicatedF

             fun copy((fds as [_], fss as [_]), _) =
                 I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
               | copy((fds, fss), I.FCOPY{tmp, ...}) =
                 I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}

             fun spillCopyTmp(_, I.FCOPY{dst,src,tmp,impl},loc) =
                 I.FCOPY{dst=dst, src=src, impl=impl,
                        tmp=SOME(I.Displace{base=sp, 
                                            disp=SpillTable.getFregLoc loc})}
   
             fun spillInstr(_, d,loc) =
                 [I.FSTORE{s=I.STDF, r=sp,
                           i=I.IMMED(SpillTable.getFregLoc loc), 
                           d=d, mem=spill}]
   
             fun reloadInstr(_, d,loc) =
                 [I.FLOAD{l=I.LDDF, r=sp, 
                          i=I.IMMED(SpillTable.getFregLoc loc), 
                          d=d, mem=spill}
                 ]

             val mode = RACore.NO_OPTIMIZATION
          end
         )
  )
