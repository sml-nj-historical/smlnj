(*
 * PPC specific backend
 *)
structure PPCCG = 
  MachineGen
  ( structure MachSpec   = PPCSpec
    structure PseudoOps  = PPCPseudoOps
    structure CpsRegs    = PPCCpsRegs
    structure InsnProps  = PPCProps
    structure Asm        = PPCAsmEmitter
    structure Shuffle    = PPCShuffle

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

    structure PPCJumps =
       PPCJumps(structure Instr=PPCInstr
                structure Shuffle=PPCShuffle)

    structure BackPatch =
       BBSched2(structure Flowgraph = PPCFlowGraph
                structure Jumps = PPCJumps
                structure Emitter = PPCMCEmitter)

    structure RA = 
       RegAlloc
         (structure I         = PPCInstr
          structure MachSpec  = PPCSpec
          structure Flowgraph = PPCFlowGraph
          structure CpsRegs   = PPCCpsRegs
          structure InsnProps = InsnProps 
          structure Rewrite   = PPCRewrite(PPCInstr) 
          structure Asm       = PPCAsmEmitter

          val sp = I.C.stackptrR
          val spill = CPSRegions.spill

          fun pure _ = false

          (* make copy *)
          fun copyR((rds as [_], rss as [_]), _) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
            | copyR((rds, rss), I.COPY{tmp, ...}) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
          fun copyF((fds as [_], fss as [_]), _) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
            | copyF((fds, fss), I.FCOPY{tmp, ...}) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}

          (* spill copy temp *)
          fun spillCopyTmp(I.COPY{dst,src,tmp,impl},offset) =
              I.COPY{dst=dst, src=src, impl=impl,
                     tmp=SOME(I.Displace{base=sp, disp=I.ImmedOp offset})}
          fun spillFcopyTmp(I.FCOPY{dst,src,tmp,impl},offset) =
              I.FCOPY{dst=dst, src=src, impl=impl,
                     tmp=SOME(I.Displace{base=sp, disp=I.ImmedOp offset})}

          (* spill register *)
          fun spillInstrR(rs,offset) =
              [I.ST{st=I.STW, ra=sp, d=I.ImmedOp offset, rs=rs, mem=spill}]
          fun spillInstrF(fs,offset) =
              [I.STF{st=I.STFD, ra=sp, d=I.ImmedOp offset, fs=fs, mem=spill}]

          (* reload register *)
          fun reloadInstrR(rt,offset,rest) =
              I.L{ld=I.LWZ, ra=sp, d=I.ImmedOp offset, rt=rt, mem=spill}::rest
          fun reloadInstrF(ft,offset,rest) =
              I.LF{ld=I.LFD, ra=sp, d=I.ImmedOp offset, ft=ft, mem=spill}::rest
         )
  )
