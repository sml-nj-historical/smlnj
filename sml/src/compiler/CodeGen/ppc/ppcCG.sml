(*
 * PPC specific backend
 *)
structure PPCCG = 
  MachineGen
  ( structure MachSpec   = PPCSpec
    structure CB	 = CellsBasis
    structure ClientPseudoOps = PPCClientPseudoOps
    structure PseudoOps  = PPCPseudoOps
    structure Ext        = SMLNJMLTreeExt(* generic extension *)
    structure CpsRegs    = PPCCpsRegs
    structure InsnProps  = PPCProps
    structure Asm        = PPCAsmEmitter
    structure Shuffle    = PPCShuffle

    structure CCalls     = DummyCCallsFn (PPCMLTree)

    structure OmitFramePtr = struct
      exception NotImplemented
      structure CFG=PPCCFG
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
		structure CFG = PPCCFG
		structure TS = PPCMLTreeStream
               )
           val bit64mode=false
           val multCost=ref 6 (* an estimate *)
         )

    structure Jumps =
       PPCJumps(structure Instr=PPCInstr
		structure MLTreeEval=PPCMLTreeEval
                structure Shuffle=PPCShuffle)

    structure BackPatch =
       BBSched2(structure CFG = PPCCFG
		structure Placement = DefaultBlockPlacement(PPCCFG)
                structure Jumps = Jumps
                structure Emitter = PPCMCEmitter)

    structure RA = 
       RISC_RA
         (structure I         = PPCInstr
          structure Flowgraph = PPCCFG
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
	      fun copy((rds, rss), I.COPY{k=CB.GP, sz, tmp, ...}) = let
		val tmp = (case (rds, rss) of ([_], [_]) => NONE | _ => tmp)
              in I.COPY{k=CB.GP, sz=sz, dst=rds, src=rss, tmp=tmp}
              end

              (* spill copy temp *)
              fun spillCopyTmp(an, I.COPY{k=CB.GP, sz, tmp,dst,src, ...},loc) =
                  I.COPY{k=CB.GP, sz=sz,  dst=dst, src=src,
			 tmp=SOME(I.Displace{base=sp, 
                                             disp=I.ImmedOp(SpillTable.getRegLoc loc)})}


              (* spill register *)
             fun spillInstr{src,spilledCell,spillLoc,an} =
                 [I.st{st=I.STW, ra=sp, 
                       d=I.ImmedOp(SpillTable.getRegLoc spillLoc),
                       rs=src, mem=spill}]
             (* reload register *)
             fun reloadInstr{dst,spilledCell,spillLoc,an} =
                 [I.l{ld=I.LWZ, ra=sp, 
                      d=I.ImmedOp(SpillTable.getRegLoc spillLoc), 
                      rt=dst, mem=spill}]

             val mode = RACore.NO_OPTIMIZATION
         end
         structure Float =
         struct
             val avail     = PPCCpsRegs.availF
             val dedicated = PPCCpsRegs.dedicatedF

	     fun copy((fds, fss), I.COPY{k=CB.FP, sz, tmp, ...}) = let
	       val tmp =(case (fds, fss) of ([_],[_]) => NONE | _ => tmp)
	     in I.COPY{k=CB.FP, sz=sz, dst=fds, src=fss, tmp=tmp}
	     end

	     fun spillCopyTmp(an, I.COPY{k=CB.FP, sz, tmp,dst,src},loc) =
		 I.COPY{k=CB.FP, sz=sz, dst=dst,src=src,
			tmp=SOME(I.Displace{base=sp, disp=I.ImmedOp(SpillTable.getFregLoc loc)})}


             fun spillInstr(_, fs,loc) =
                 [I.stf{st=I.STFD, ra=sp, 
                        d=I.ImmedOp(SpillTable.getFregLoc loc), 
                        fs=fs, mem=spill}]
   
             fun reloadInstr(_, ft,loc) =
                 [I.lf{ld=I.LFD, ra=sp, d=I.ImmedOp(SpillTable.getFregLoc loc),
                       ft=ft, mem=spill}]

             val mode = RACore.NO_OPTIMIZATION
         end
        )
  )
