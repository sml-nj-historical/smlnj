(*
 * Sparc specific backend
 *)
structure SparcCG = 
  MachineGen
  ( structure MachSpec   = SparcSpec
    structure PseudoOps  = SparcPseudoOps
    structure CpsRegs    = SparcCpsRegs
    structure InsnProps  = SparcProps
    structure Asm        = SparcAsmEmitter
    structure Shuffle    = SparcShuffle

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

    structure SparcJumps =
       SparcJumps(structure Instr=SparcInstr
                  structure Shuffle=SparcShuffle)

    structure BackPatch =
       SpanDependencyResolution
         (structure Flowgraph = SparcFlowGraph
          structure Jumps     = SparcJumps
          structure Emitter   = SparcMCEmitter
          structure DelaySlot = SparcDelaySlots
             (structure I=SparcInstr
              structure P=InsnProps)
          structure Props = InsnProps
         )

    structure RA = 
       RegAlloc
         (structure I         = SparcInstr
          structure MachSpec  = SparcSpec
          structure Flowgraph = SparcFlowGraph
          structure CpsRegs   = SparcCpsRegs
          structure InsnProps = InsnProps 
          structure Rewrite   = SparcRewrite(SparcInstr)
          structure Asm       = SparcAsmEmitter

          val sp = I.C.stackptrR
          val spill = CPSRegions.spill
         
          fun pure(I.ANNOTATION{i,...}) = pure i
            | pure(I.LOAD _) = true
            | pure(I.FLOAD _) = true
            | pure(I.SETHI _) = true
            | pure(I.SHIFT _) = true
            | pure(I.FPop1 _) = true
            | pure(I.FPop2 _) = true
            | pure _ = false

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
                     tmp=SOME(I.Displace{base=sp, disp=offset})}
          fun spillFcopyTmp(I.FCOPY{dst,src,tmp,impl},offset) =
              I.FCOPY{dst=dst, src=src, impl=impl,
                     tmp=SOME(I.Displace{base=sp, disp=offset})}

          (* spill register *)
          fun spillInstrR(d,offset) =
              [I.STORE{s=I.ST, r=sp, i=I.IMMED offset, d=d, mem=spill}]
          fun spillInstrF(d,offset) =
              [I.FSTORE{s=I.STDF, r=sp, i=I.IMMED offset, d=d, mem=spill}]

          (* reload register *)
          fun reloadInstrR(d,offset,rest) =
              I.LOAD{l=I.LD, r=sp, i=I.IMMED offset, d=d, mem=spill}::rest
          fun reloadInstrF(d,offset,rest) =
              I.FLOAD{l=I.LDDF, r=sp, i=I.IMMED offset, d=d, mem=spill}::rest
         )
  )
