(*
 * Alpha32 specific backend
 *)
structure Alpha32CG = 
  MachineGen
  ( structure I          = Alpha32Instr
    structure MachSpec   = Alpha32Spec
    structure PseudoOps  = Alpha32PseudoOps
    structure Ext        = SMLNJMLTreeExt(* generic extension *)
    structure CpsRegs    = Alpha32CpsRegs
    structure InsnProps  = Alpha32Props
    structure Asm        = Alpha32AsmEmitter
    structure Shuffle    = Alpha32Shuffle

    structure CCalls     = DummyCCallsFn (Alpha32MLTree)

    structure MLTreeComp=
       Alpha(structure AlphaInstr = Alpha32Instr
             structure AlphaMLTree = Alpha32MLTree
             structure PseudoInstrs = Alpha32PseudoInstrs
             structure ExtensionComp = SMLNJMLTreeExtComp
               (structure I = Alpha32Instr
                structure T = Alpha32MLTree
               )
             val mode32bit = true (* simulate 32 bit mode *)
             val multCost = ref 8 (* just guessing *)
             val useMultByConst = ref false (* just guessing *)
             val byteWordLoadStores = ref false
             val SMLNJfloatingPoint = true (* must be true for SML/NJ *)
            )

    structure Jumps =
       AlphaJumps(structure Instr=Alpha32Instr
                  structure Shuffle=Alpha32Shuffle)

    structure BackPatch =
       BBSched2(structure Flowgraph = Alpha32FlowGraph
                structure Jumps = Jumps
                structure Emitter = Alpha32MCEmitter)

    structure RA = 
       RISC_RA
         (structure I         = Alpha32Instr
          structure Flowgraph = Alpha32FlowGraph
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
                  I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
                | copy((rds, rss), I.COPY{tmp, ...}) =
                  I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}

              (* spill copy temp *)
              fun spillCopyTmp(an, I.COPY{tmp,dst,src,impl},loc) =
                  I.COPY{tmp=SOME(I.Displace{base=sp, 
                                             disp=SpillTable.getRegLoc loc}),
                         dst=dst,src=src,impl=impl}

              (* spill register *)
              fun spillInstr(_, r,loc) =
                  [I.STORE{stOp=I.STL, b=sp,
                           d=I.IMMop(SpillTable.getRegLoc loc), 
                           r=r, mem=spill}]

              (* reload register *)
              fun reloadInstr(_, r,loc) =
                  [I.LOAD{ldOp=I.LDL, b=sp, 
                          d=I.IMMop(SpillTable.getRegLoc loc),
                          r=r, mem=spill}]
          end
 
          structure Float =   
          struct
              val avail     = Alpha32CpsRegs.availF
              val dedicated = Alpha32CpsRegs.dedicatedF

              fun copy((fds as [_], fss as [_]), _) =
                  I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
                | copy((fds, fss), I.FCOPY{tmp, ...}) =
                  I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}

              fun spillCopyTmp(an, I.FCOPY{tmp,dst,src,impl},loc) =
                  I.FCOPY{tmp=SOME(I.Displace{base=sp, 
                                          disp=SpillTable.getFregLoc loc}),
                          dst=dst,src=src,impl=impl}

              fun spillInstr(_, r,loc) =
                  [I.FSTORE{stOp=I.STT, b=sp, 
                            d=I.IMMop(SpillTable.getFregLoc loc), 
                            r=r, mem=spill}]

              fun reloadInstr(_, r,loc) =
                  [I.FLOAD{ldOp=I.LDT, b=sp,
                           d=I.IMMop(SpillTable.getFregLoc loc), 
                           r=r, mem=spill}]
          end
         )
  )
