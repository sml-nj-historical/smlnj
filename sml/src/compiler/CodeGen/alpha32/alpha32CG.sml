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
    structure CB = CellsBasis
      

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

	      fun copy((rds, rss), I.COPY{k=CB.GP, sz, tmp, ...}) = let
		val tmp = (case (rds, rss) of ([_], [_]) => NONE | _ => tmp)
              in I.COPY{k=CB.GP, sz=sz, dst=rds, src=rss, tmp=tmp}
              end

              (* spill copy temp *)
              fun spillCopyTmp(an, I.COPY{k=CB.GP, sz, tmp,dst,src, ...},loc) =
                  I.COPY{k=CB.GP, sz=sz,  dst=dst,src=src,
			 tmp=SOME(I.Displace{base=sp, 
                                             disp=SpillTable.getRegLoc loc})}

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

	      fun copy((fds, fss), I.COPY{k=CB.FP, sz, tmp, ...}) = let
		val tmp =(case (fds, fss) of ([_],[_]) => NONE | _ => tmp)
              in I.COPY{k=CB.FP, sz=sz, dst=fds, src=fss, tmp=tmp}
              end

              fun spillCopyTmp(an, I.COPY{k=CB.FP, sz, tmp,dst,src},loc) =
                  I.COPY{k=CB.FP, sz=sz, dst=dst,src=src,
			 tmp=SOME(I.Displace{base=sp, disp=SpillTable.getFregLoc loc})}
                          

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
