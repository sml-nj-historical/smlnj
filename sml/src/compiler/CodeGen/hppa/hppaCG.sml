(*
 * Hppa specific backend
 *)
structure HppaCG = 
  MachineGen
  ( structure MachSpec   = HppaSpec
    structure PseudoOps  = HppaPseudoOps
    structure CpsRegs    = HppaCpsRegs
    structure InsnProps  = HppaProps
    structure Asm        = HppaAsmEmitter
    structure Shuffle    = HppaShuffle

    structure HppaMillicode =
      HppaMillicode(structure MLTree=HppaMLTree
                    structure Instr=HppaInstr)

    structure HppaLabelComp =
      HppaLabelComp(structure MLTree=HppaMLTree
                    structure Instr=HppaInstr)

    structure MLTreeComp=
       Hppa(structure HppaInstr = HppaInstr
            structure HppaMLTree = HppaMLTree
            structure MilliCode=HppaMillicode
            structure LabelComp=HppaLabelComp
            structure ExtensionComp = SMLNJMLTreeExtComp
               (structure I = HppaInstr
                structure T = HppaMLTree
               )
            val costOfMultiply = ref 7
            val costOfDivision = ref 7
           )

    structure HppaJumps =
       HppaJumps(structure Instr=HppaInstr
                 structure Shuffle=HppaShuffle)

    structure BackPatch =
       SpanDependencyResolution
         (structure Flowgraph = HppaFlowGraph
          structure Jumps     = HppaJumps
          structure Emitter   = HppaMCEmitter
          structure DelaySlot = HppaDelaySlots
             (structure I=HppaInstr
              structure P=InsnProps)
          structure Props = InsnProps
         )

    structure RA = 
       RegAlloc
         (structure I         = HppaInstr
          structure MachSpec  = HppaSpec
          structure Flowgraph = HppaFlowGraph
          structure CpsRegs   = HppaCpsRegs
          structure InsnProps = InsnProps 
          structure Rewrite   = HppaRewrite(HppaInstr) 
          structure Asm       = HppaAsmEmitter

          (* NOTE: the spill offset grows backwards on the stack! 
           *)

          val sp        = I.C.stackptrR
          val spill     = CPSRegions.spill
          val tmpR      = I.C.asmTmpR
          val itow      = Word.fromInt
          val wtoi      = Word.toIntX
          fun low11(n)  = wtoi(Word.andb(itow n, 0wx7ff))
          fun high21(n) = wtoi(Word.~>>(itow n, 0w11))

          fun pure(I.LOAD _) = true
            | pure(I.LOADI _) = true
            | pure(I.FLOAD _) = true
            | pure(I.FLOADX _) = true
            | pure(I.ARITH _) = true
            | pure(I.ARITHI _) = true
            | pure(I.FARITH _) = true
            | pure(I.FUNARY _) = true
            | pure(I.FCNV _) = true
            | pure(I.ANNOTATION{i,...}) = pure i
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
                     tmp=SOME(I.Displace{base=sp, disp= ~offset})}
          fun spillFcopyTmp(I.FCOPY{dst,src,tmp,impl},offset) =
              I.FCOPY{dst=dst, src=src, impl=impl,
                     tmp=SOME(I.Displace{base=sp, disp= ~offset})}

          (* spill register *) 
          fun spillInstrR(r,offset) =
              [I.STORE{st=I.STW, b=sp, d=I.IMMED(~offset), r=r, mem=spill}]
          fun spillInstrF(r,offset) =
              [I.LDIL{i=I.IMMED(high21(~offset)), t=tmpR},
               I.LDO{i=I.IMMED(low11(~offset)), b=tmpR, t=tmpR},
               I.FSTOREX{fstx=I.FSTDX, b=sp, x=tmpR, r=r, mem=spill}
              ]

          (* reload register *) 
          fun reloadInstrR(t,offset,rest) =
              I.LOADI{li=I.LDW, i=I.IMMED(~offset), r=sp, t=t, mem=spill}::rest
          fun reloadInstrF(t,offset,rest) =
              I.LDIL{i=I.IMMED(high21(~offset)), t=tmpR} ::
              I.LDO{i=I.IMMED(low11(~offset)), b=tmpR, t=tmpR} ::
              I.FLOADX{flx=I.FLDDX, b=sp, x=tmpR, t=t, mem=spill} :: rest
         )
  )
