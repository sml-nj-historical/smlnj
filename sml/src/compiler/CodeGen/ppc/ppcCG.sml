(*
 * PPC specific backend
 *)
structure PPCCG = 
  MachineGen
  ( structure MachSpec   = PPCSpec
    structure PseudoOps  = PPCPseudoOps
    structure Ext        = SMLNJMLTreeExt(* generic extension *)
    structure CpsRegs    = PPCCpsRegs
    structure InsnProps  = PPCProps
    structure Asm        = PPCAsmEmitter
    structure Shuffle    = PPCShuffle

    structure CCalls     = DummyCCallsFn (PPCMLTree)

    structure OmitFramePtr = struct
      exception NotImplemented
      structure F=PPCFlowGraph
      structure I=PPCInstr
      val vfp = PPCCpsRegs.vfp
      fun omitframeptr _ = raise NotImplemented
    end

    structure MLTreeComp=
       PPC(structure PPCInstr = PPCInstr
           structure PPCMLTree = PPCMLTree
           structure PseudoInstrs=
               PPCPseudoInstr(structure Instr=PPCInstr)
           structure ExtensionComp = SMLNJMLTreeExtComp
               (structure I = PPCInstr
                structure T = PPCMLTree
               )
           val bit64mode=false
           val multCost=ref 6 (* an estimate *)
         )

    structure Jumps =
       PPCJumps(structure Instr=PPCInstr
                structure Shuffle=PPCShuffle)

    structure BackPatch =
       BBSched2(structure Flowgraph = PPCFlowGraph
                structure Jumps = Jumps
                structure Emitter = PPCMCEmitter)

    structure RA = 
       RISC_RA
         (structure I         = PPCInstr
          structure Flowgraph = PPCFlowGraph
          structure CpsRegs   = PPCCpsRegs
          structure InsnProps = InsnProps 
          structure Rewrite   = PPCRewrite(PPCInstr) 
          structure Asm       = PPCAsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = PPCAsmEmitter)

          structure SpillTable = SpillTable(PPCSpec)

          val architecture = PPCSpec.architecture

          val beginRA = SpillTable.spillInit

          val sp = I.C.stackptrR
          val spill = CPSRegions.spill

          fun pure _ = false

          structure Int = 
          struct
             val avail     = PPCCpsRegs.availR
             val dedicated = PPCCpsRegs.dedicatedR

             (* make copy *)
             fun copy((rds as [_], rss as [_]), _) =
                 I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
               | copy((rds, rss), I.COPY{tmp, ...}) =
                 I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}

             (* spill copy temp *)
             fun spillCopyTmp(_, I.COPY{dst,src,tmp,impl},loc) =
                 I.COPY{dst=dst, src=src, impl=impl,
                        tmp=SOME(I.Displace
                                   {base=sp, 
                                    disp=I.ImmedOp(SpillTable.getRegLoc loc)})}

              (* spill register *)
             fun spillInstr{src,spilledCell,spillLoc,an} =
                 [I.ST{st=I.STW, ra=sp, 
                       d=I.ImmedOp(SpillTable.getRegLoc spillLoc),
                       rs=src, mem=spill}]
             (* reload register *)
             fun reloadInstr{dst,spilledCell,spillLoc,an} =
                 [I.L{ld=I.LWZ, ra=sp, 
                      d=I.ImmedOp(SpillTable.getRegLoc spillLoc), 
                      rt=dst, mem=spill}]

             val mode = RACore.NO_OPTIMIZATION
         end
         structure Float =
         struct
             val avail     = PPCCpsRegs.availF
             val dedicated = PPCCpsRegs.dedicatedF

             fun copy((fds as [_], fss as [_]), _) =
                 I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
               | copy((fds, fss), I.FCOPY{tmp, ...}) =
                 I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}
   
             fun spillCopyTmp(_, I.FCOPY{dst,src,tmp,impl},loc) =
                 I.FCOPY{dst=dst, src=src, impl=impl,
                        tmp=SOME(I.Displace
                                   {base=sp, 
                                    disp=I.ImmedOp(SpillTable.getFregLoc loc)
                                   })}
   
             fun spillInstr(_, fs,loc) =
                 [I.STF{st=I.STFD, ra=sp, 
                        d=I.ImmedOp(SpillTable.getFregLoc loc), 
                        fs=fs, mem=spill}]
   
             fun reloadInstr(_, ft,loc) =
                 [I.LF{ld=I.LFD, ra=sp, d=I.ImmedOp(SpillTable.getFregLoc loc),
                       ft=ft, mem=spill}]

             val mode = RACore.NO_OPTIMIZATION
         end
        )
  )
