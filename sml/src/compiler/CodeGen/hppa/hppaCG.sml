(*
 * Hppa specific backend
 *)
structure HppaCG = 
  MachineGen
  ( structure MachSpec   = HppaSpec
    structure ClientPseudoOps = HppaClientPseudoOps
    structure PseudoOps  = HppaPseudoOps
    structure Ext        = SMLNJMLTreeExt(* generic extension *)
    structure CpsRegs    = HppaCpsRegs
    structure InsnProps  = HppaProps
    structure Asm        = HppaAsmEmitter
    structure Shuffle    = HppaShuffle

    structure CCalls     = DummyCCallsFn (HppaMLTree)

    structure OmitFramePtr = struct
      exception NotImplemented
      structure CFG=HppaCFG
      structure I=HppaInstr
      val vfp = CpsRegs.vfp
      fun omitframeptr _ = raise NotImplemented
    end

    structure HppaMillicode = HppaMillicode(HppaInstr)

    structure HppaLabelComp = HppaLabelComp(HppaInstr)

    structure MLTreeComp=
       Hppa(structure HppaInstr = HppaInstr
            structure HppaMLTree = HppaMLTree
            structure MilliCode=HppaMillicode
            structure LabelComp=HppaLabelComp
            structure ExtensionComp = SMLNJMLTreeExtComp
               (structure I = HppaInstr
                structure T = HppaMLTree
		structure CFG = HppaCFG
		structure TS = HppaMLTreeStream
               )
            val costOfMultiply = ref 7
            val costOfDivision = ref 7
           )

    structure Jumps =
       HppaJumps(structure Instr=HppaInstr
		 structure MLTreeEval=HppaMLTreeEval
                 structure Shuffle=HppaShuffle)

    structure BackPatch =
       SpanDependencyResolution
         (structure CFG = HppaCFG
	  structure Placement = DefaultBlockPlacement(HppaCFG)
          structure Jumps     = Jumps
          structure Emitter   = HppaMCEmitter
          structure DelaySlot = HppaDelaySlots
             (structure I=HppaInstr
              structure P=InsnProps)
          structure Props = InsnProps
         )

    structure RA = 
       RISC_RA
         (structure I         = HppaInstr
          structure Flowgraph = HppaCFG
          structure InsnProps = InsnProps 
          structure Rewrite   = HppaRewrite(HppaInstr) 
          structure Asm       = HppaAsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = HppaAsmEmitter)

          (* NOTE: the spill offset grows backwards on the stack! 
           *)
          structure SpillTable = SpillTable(HppaSpec)

          val beginRA = SpillTable.spillInit

          val architecture = HppaSpec.architecture

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
          structure Int =
          struct
             val avail = HppaCpsRegs.availR
             val dedicated = HppaCpsRegs.dedicatedR

             fun copy((rds as [_], rss as [_]), _) =
                 I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
               | copy((rds, rss), I.COPY{tmp, ...}) =
                 I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}

             (* spill copy temp *) 
             fun spillCopyTmp(_, I.COPY{dst,src,tmp,impl},loc) =
                 I.COPY{dst=dst, src=src, impl=impl,
                        tmp=SOME(I.Displace{base=sp, 
                                           disp= ~(SpillTable.getRegLoc loc)})}

             (* spill register *) 
             fun spillInstr{src,spilledCell,spillLoc,an} =
                 [I.STORE{st=I.STW, b=sp, 
                          d=I.IMMED(~(SpillTable.getRegLoc spillLoc)), 
                          r=src, mem=spill}]

             (* reload register *) 
             fun reloadInstr{dst,spilledCell,spillLoc,an} =
                 [I.LOADI{li=I.LDW, 
                          i=I.IMMED(~(SpillTable.getRegLoc spillLoc)), 
                          r=sp, t=dst, mem=spill}
                 ]

             val mode = RACore.NO_OPTIMIZATION
          end

          structure Float = 
          struct
             val avail = HppaCpsRegs.availF
             val dedicated = HppaCpsRegs.dedicatedF
 
             fun copy((fds as [_], fss as [_]), _) =
                 I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
               | copy((fds, fss), I.FCOPY{tmp, ...}) =
                 I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}
   
             fun spillCopyTmp(_,I.FCOPY{dst,src,tmp,impl},loc) =
                 I.FCOPY{dst=dst, src=src, impl=impl,
                        tmp=SOME(I.Displace{base=sp, 
                                        disp= ~(SpillTable.getFregLoc loc)})}
   
             fun spillInstr(_,r,loc) =
             let val offset = SpillTable.getFregLoc loc
             in  [I.LDIL{i=I.IMMED(high21(~offset)), t=tmpR},
                  I.LDO{i=I.IMMED(low11(~offset)), b=tmpR, t=tmpR},
                  I.FSTOREX{fstx=I.FSTDX, b=sp, x=tmpR, r=r, mem=spill}
                 ]
             end
   
             fun reloadInstr(_,t,loc) =
             let val offset = SpillTable.getFregLoc loc
             in  [I.LDIL{i=I.IMMED(high21(~offset)), t=tmpR}, 
                  I.LDO{i=I.IMMED(low11(~offset)), b=tmpR, t=tmpR},
                  I.FLOADX{flx=I.FLDDX, b=sp, x=tmpR, t=t, mem=spill} 
                 ]
             end

             val mode = RACore.NO_OPTIMIZATION
          end
         )
  )
