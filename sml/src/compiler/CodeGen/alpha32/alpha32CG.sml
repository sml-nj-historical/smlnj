(*
 * Alpha32 specific backend
 *)
structure Alpha32CG = 
  MachineGen
  ( structure I          = Alpha32Instr
    structure MachSpec   = Alpha32Spec
    structure ClientPseudoOps = Alpha32ClientPseudoOps
    structure PseudoOps  = Alpha32PseudoOps
    structure Ext        = SMLNJMLTreeExt(* generic extension *)
    structure CpsRegs    = Alpha32CpsRegs
    structure InsnProps  = Alpha32Props
    structure Asm        = Alpha32AsmEmitter
    structure Shuffle    = Alpha32Shuffle
   
    structure CCalls     = DummyCCallsFn (Alpha32MLTree)
    structure OmitFramePtr = struct
      exception NotImplemented
      structure CFG=Alpha32CFG
      structure I=Alpha32Instr
      val vfp = CpsRegs.vfp
      fun omitframeptr _ = raise NotImplemented
    end
      

    structure MLTreeComp=
       Alpha(structure AlphaInstr = Alpha32Instr
             structure AlphaMLTree = Alpha32MLTree
             structure PseudoInstrs = Alpha32PseudoInstrs
             structure ExtensionComp = SMLNJMLTreeExtComp
               (structure I = Alpha32Instr
                structure T = Alpha32MLTree
		structure CFG = Alpha32CFG
		structure TS = Alpha32MLTreeStream
               )
             val mode32bit = true (* simulate 32 bit mode *)
             val multCost = ref 8 (* just guessing *)
             val useMultByConst = ref false (* just guessing *)
             val byteWordLoadStores = ref false
             val SMLNJfloatingPoint = true (* must be true for SML/NJ *)
            )

    structure Jumps =
       AlphaJumps(structure Instr=Alpha32Instr
                  structure Shuffle=Alpha32Shuffle
		  structure MLTreeEval=Alpha32MLTreeEval)

    structure BackPatch =
       BBSched2(structure CFG=Alpha32CFG
                structure Jumps = Jumps
		structure Placement = DefaultBlockPlacement(Alpha32CFG)
                structure Emitter = Alpha32MCEmitter)

    structure RA = 
       RISC_RA
         (structure I         = Alpha32Instr
          structure Flowgraph = Alpha32CFG
          structure InsnProps = InsnProps 
          structure Rewrite   = AlphaRewrite(Alpha32Instr)
          structure Asm       = Alpha32AsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = Alpha32AsmEmitter)

          val sp    = I.C.stackptrR
          val spill = CPSRegions.spill

          structure SpillTable = SpillTable(Alpha32Spec)

          val architecture = Alpha32Spec.architecture

          val beginRA = SpillTable.spillInit

          fun pure _ = false

          (* make copies *)
          structure Int = 
          struct
              val avail     = Alpha32CpsRegs.availR
              val dedicated = Alpha32CpsRegs.dedicatedR

              fun copy((rds as [_], rss as [_]), _) =
                  I.copy{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
                | copy((rds, rss), I.INSTR(I.COPY{tmp, ...})) =
                  I.copy{dst=rds, src=rss, impl=ref NONE, tmp=tmp}

              (* spill copy temp *)
              fun spillCopyTmp(an, I.INSTR(I.COPY{tmp,dst,src,impl}),loc) =
                  I.copy{tmp=SOME(I.Displace{base=sp, 
                                             disp=SpillTable.getRegLoc loc}),
                         dst=dst,src=src,impl=impl}

              (* spill register *)
              fun spillInstr{src,spilledCell,spillLoc,an} =
                  [I.store{stOp=I.STL, b=sp,
                           d=I.IMMop(SpillTable.getRegLoc spillLoc), 
                           r=src, mem=spill}]

              (* reload register *)
              fun reloadInstr{dst,spilledCell,spillLoc,an} =
                  [I.load{ldOp=I.LDL, b=sp, 
                          d=I.IMMop(SpillTable.getRegLoc spillLoc),
                          r=dst, mem=spill}]

              val mode = RACore.NO_OPTIMIZATION
          end
 
          structure Float =   
          struct
              val avail     = Alpha32CpsRegs.availF
              val dedicated = Alpha32CpsRegs.dedicatedF

              fun copy((fds as [_], fss as [_]), _) =
                  I.fcopy{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
                | copy((fds, fss), I.INSTR(I.FCOPY{tmp, ...})) =
                  I.fcopy{dst=fds, src=fss, impl=ref NONE, tmp=tmp}

              fun spillCopyTmp(an, I.INSTR(I.FCOPY{tmp,dst,src,impl}),loc) =
                  I.fcopy{tmp=SOME(I.Displace{base=sp, 
                                          disp=SpillTable.getFregLoc loc}),
                          dst=dst,src=src,impl=impl}

              fun spillInstr(_, r,loc) =
                  [I.fstore{stOp=I.STT, b=sp, 
                            d=I.IMMop(SpillTable.getFregLoc loc), 
                            r=r, mem=spill}]

              fun reloadInstr(_, r,loc) =
                  [I.fload{ldOp=I.LDT, b=sp,
                           d=I.IMMop(SpillTable.getFregLoc loc), 
                           r=r, mem=spill}]

              val mode = RACore.NO_OPTIMIZATION
          end
         )
  )
