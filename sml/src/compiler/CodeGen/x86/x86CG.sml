(*
 * X86 specific backend.  This one uses the new RA8 scheme.
 *)
structure X86CG = 
  MachineGen
  ( structure I          = X86Instr
    structure F          = X86FlowGraph
    structure R          = X86CpsRegs
    structure CG         = Control.CG

    structure MachSpec   = X86Spec
    structure PseudoOps  = X86PseudoOps
    structure CpsRegs    = X86CpsRegs
    structure InsnProps  = X86Props(X86Instr)
    structure Asm        = X86AsmEmitter

    val spill = CPSRegions.spill 
    val stack = CPSRegions.stack 
    val esp   = I.C.esp

    fun error msg = MLRiscErrorMsg.error("X86CG",msg)

    (* Should we target memory registers? *)
    val targetMemoryRegisters = true

    structure MLTreeComp=
       X86(structure X86Instr=X86Instr
           structure X86MLTree=X86MLTree
           val tempMem=I.Displace{base=esp, disp=I.Immed 304, mem=stack}
          )

    structure X86Jumps = 
       X86Jumps(structure Instr=X86Instr
                structure AsmEmitter=X86AsmEmitter
                structure Shuffle=X86Shuffle
                structure MCEmitter=X86MCEmitter)
   
    structure BackPatch = 
       BackPatch(structure Jumps=X86Jumps
                 structure Emitter=X86MCEmitter
                 structure Props=InsnProps
                 structure Flowgraph=X86FlowGraph
                 structure Asm=X86AsmEmitter
                 structure CodeString=CodeString)

    structure PrintFlowGraph=
       PrintClusterFn(structure Flowgraph=X86FlowGraph
                      structure Asm = X86AsmEmitter)

    structure X86Spill = X86Spill(structure Instr=I structure Props=InsnProps)

    val toInt32 = Int32.fromInt
    fun cacheOffset r = I.Immed(toInt32(X86Runtime.vregStart + 
                                Word.toIntX(Word.<<(Word.fromInt(r-8),0w2))))

    val memRegsUsed = ref 0w0 : Word32.word ref

    fun ea r = 
        (memRegsUsed := Word32.orb(Word32.<<(0w1, Word.fromInt r),!memRegsUsed);
         if targetMemoryRegisters then I.MemReg r 
         else I.Displace{base=esp, disp=cacheOffset r, mem=stack}
        )
 
    structure X86RewritePseudo=
       X86RewritePseudo(structure Instr=X86Instr
                        structure Flowgraph=X86FlowGraph
                        structure Shuffle=X86Shuffle
                        val ea = ea
                       )


    val intSpillCnt = MLRiscControl.getCounter "ra-int-spills"
    val floatSpillCnt = MLRiscControl.getCounter "ra-float-spills"
    val intReloadCnt = MLRiscControl.getCounter "ra-int-reloads"
    val floatReloadCnt = MLRiscControl.getCounter "ra-float-reloads"
    val intRenameCnt = MLRiscControl.getCounter "ra-int-renames"
    val floatRenameCnt = MLRiscControl.getCounter "ra-float-renames"
    val x86CfgDebugFlg = MLRiscControl.getFlag "x86-cfg-debug"
(*
    val deadcode = MLRiscControl.getCounter "x86-dead-code"
    val deadblocks = MLRiscControl.getCounter "x86-dead-blocks"
 *)

    structure RA = 
    struct
      structure F = F

      (* For dead code elimination *)
      exception X86DeadCode
      val affectedBlocks = Intmap.new(32,X86DeadCode) : bool Intmap.intmap
      val deadRegs       = Intmap.new(32,X86DeadCode) : bool Intmap.intmap
      fun removeDeadCode(F.CLUSTER{blocks, ...}) =
      let val isDead = Intmap.mapWithDefault(deadRegs, false) 
          val isAffected = Intmap.mapWithDefault(affectedBlocks, false) 
          fun isDeadInstr(I.ANNOTATION{i, ...}) = isDeadInstr i 
            | isDeadInstr(I.MOVE{dst=I.Direct rd, ...}) = isDead rd
            | isDeadInstr(I.COPY{dst=[rd], ...}) = isDead rd
            | isDeadInstr _ = false
          fun scan [] = ()
            | scan(F.BBLOCK{blknum, insns, ...}::rest) =
              (if isAffected blknum then 
                  ((* deadblocks := !deadblocks + 1; *)
                   insns := elim(!insns, [])
                  ) else ();
               scan rest)
            | scan(_::rest) = scan rest
         and elim([], code) = rev code
           | elim(i::instrs, code) = 
            if isDeadInstr i then 
               ((* deadcode := !deadcode + 1; *) elim(instrs, code))
            else elim(instrs, i::code)
      in if Intmap.elems affectedBlocks > 0 then scan blocks else ()
      end
    
      (* This is the generic register allocator *)
      structure Ra = 
        RegisterAllocator
         (ChowHennessySpillHeur)
         (RADeadCodeElim
            (ClusterRA
               (structure Flowgraph = F
                structure Asm = X86AsmEmitter
                structure InsnProps = InsnProps
               )
            )
            (fun cellkind I.C.GP = true | cellkind _ = false
             val deadRegs = deadRegs
             val affectedBlocks = affectedBlocks
            )
         )


      (* -------------------------------------------------------------------
       * Floating point stuff 
       * -------------------------------------------------------------------*)
      structure FR = GetReg(val nRegs=32 
                            val available=R.availF 
                            val first=32)

      val KF = length R.availF
  
      fun copyInstrF((rds as [_], rss as [_]), _) =
            [I.FCOPY{dst=rds, src=rss, tmp=NONE}]
        | copyInstrF((rds, rss), I.FCOPY{tmp, ...}) = 
            [I.FCOPY{dst=rds, src=rss, tmp=tmp}]
  
      fun getFregLoc loc = 
          I.Displace{base=esp, disp=X86StackSpills.getFregLoc loc, mem=spill}
  
      (* spill floating point *)
      fun spillF{instr, reg, spillLoc, kill, regmap, annotations} = 
          (floatSpillCnt := !floatSpillCnt + 1;
           X86Spill.fspill(instr, regmap, reg, getFregLoc spillLoc) 
          )
  
      fun spillFreg{src, reg, spillLoc, annotations} = 
         (floatSpillCnt := !floatSpillCnt + 1;
          [I.FLD(I.FDirect(src)), I.FSTP(getFregLoc spillLoc)]
         )

      fun spillFcopyTmp{copy=I.FCOPY{dst, src, ...}, spillLoc, annotations} =
          (floatSpillCnt := !floatSpillCnt + 1;
           I.FCOPY{dst=dst, src=src, tmp=SOME(getFregLoc spillLoc)}
          )

      (* rename floating point *)
      fun renameF{instr, fromSrc, toSrc, regmap} =
          (floatRenameCnt := !floatRenameCnt + 1;
           X86Spill.freload(instr, regmap, fromSrc, I.FDirect toSrc)
          )

      (* reload floating point *)
      fun reloadF{instr, reg, spillLoc, regmap, annotations} = 
          (floatReloadCnt := !floatReloadCnt + 1;
           X86Spill.freload(instr, regmap, reg, getFregLoc spillLoc)
          )

      fun reloadFreg{dst, reg, spillLoc, annotations} = 
          (floatReloadCnt := !floatReloadCnt + 1;
           [I.FLD(getFregLoc spillLoc), I.FSTP(I.FDirect dst)]
          )

      (* -------------------------------------------------------------------
       * Integer 8 stuff 
       * -------------------------------------------------------------------*)
      fun memToMemMove{dst, src} =
          let val tmp = I.C.newReg() 
          in  [I.MOVE{mvOp=I.MOVL,src=src,dst=I.Direct tmp},
               I.MOVE{mvOp=I.MOVL,src=I.Direct tmp,dst=dst}
              ]
          end

      fun copyInstrR((rds as [d], rss as [s]), _) =
          if d = s then [] else 
          (case (d >= 8 andalso d < 32, s >= 8 andalso s < 32) of
            (false, false) => [I.COPY{dst=rds, src=rss, tmp=NONE}]
          | (true, false) => [I.MOVE{mvOp=I.MOVL,src=I.Direct s,dst=I.MemReg d}]
          | (false, true) => [I.MOVE{mvOp=I.MOVL,src=I.MemReg s,dst=I.Direct d}]
          | (true, true) => memToMemMove{src=I.MemReg s, dst=I.MemReg d}
          )
        | copyInstrR((rds, rss), I.COPY{tmp, ...}) = 
           [I.COPY{dst=rds, src=rss, tmp=tmp}]
  
      val getRegLoc' = X86StackSpills.getRegLoc

      val firstSpill = ref true

      fun getRegLoc(spillLoc) = 
          (* Is it a memory register? *)
          if spillLoc >= 0 then I.MemReg spillLoc else
          (* No, logical spill locations... *)
          (if !firstSpill then
             (* find out what pseudo memory registers are not used and can be 
              * used for spill space. 
              *) 
             (firstSpill := false;
              let fun loop(m,r,free) =
                      if r >= 10 then (* note, 8 and 9 are reserved! *)
                        if Word32.andb(m,Word32.<<(0w1,Word.fromInt r)) <> 0w0
                        then loop(m,r-1,free)
                        else ((* print ("["^Int.toString r^"]"); *)
                              loop(m,r-1,cacheOffset r::free))
                      else free
                  val m    = !memRegsUsed
                  val free = loop(m, 31 (* X86Runtime.numVregs+8-1 *), [])
              in  X86StackSpills.setAvailableOffsets free
              end 
             ) else ();
           I.Displace{base=esp, disp=getRegLoc' spillLoc, mem=spill}
          )
  
      structure GR8 = GetReg(val nRegs=8 val available=X86CpsRegs.availR
                             val first=0)
   
      val K8 = length X86CpsRegs.availR
  
       (* register allocation for general purpose registers *)
      fun spillR8{instr, reg, spillLoc, kill, regmap, annotations} = 
          (intSpillCnt := !intSpillCnt + 1;
           X86Spill.spill(instr, regmap, reg, getRegLoc spillLoc)
          ) 

      fun spillReg{src, reg, spillLoc, annotations} = 
          let val _ = intSpillCnt := !intSpillCnt + 1;
              val dstLoc = getRegLoc spillLoc
              val isMemReg = src >= 8 andalso src < 32 
              val srcLoc = if isMemReg then I.MemReg src else I.Direct src
          in  if InsnProps.eqOpn(srcLoc, dstLoc) then []
              else if isMemReg then memToMemMove{dst=dstLoc, src=srcLoc}
              else [I.MOVE{mvOp=I.MOVL, src=srcLoc, dst=dstLoc}]
          end

      fun spillCopyTmp{copy=I.COPY{src, dst,...}, spillLoc, annotations} = 
          (intSpillCnt := !intSpillCnt + 1;
           I.COPY{dst=dst, src=src, tmp=SOME(getRegLoc spillLoc)}
          )
     
      fun renameR8{instr, fromSrc, toSrc, regmap} = 
          (intRenameCnt := !intRenameCnt + 1;
           X86Spill.reload(instr, regmap, fromSrc, I.Direct toSrc)
          )

      fun reloadR8{instr, reg, spillLoc, regmap, annotations} = 
          (intReloadCnt := !intReloadCnt + 1;
           X86Spill.reload(instr, regmap, reg, getRegLoc spillLoc)
          ) 

      fun reloadReg{dst, reg, spillLoc, annotations} = 
          let val _ = intReloadCnt := !intReloadCnt + 1
              val srcLoc = getRegLoc spillLoc
              val isMemReg = dst >= 8 andalso dst < 32 
              val dstLoc = if isMemReg then I.MemReg dst else I.Direct dst
          in  if InsnProps.eqOpn(srcLoc,dstLoc) then []
              else if isMemReg then memToMemMove{dst=dstLoc, src=srcLoc}
              else [I.MOVE{mvOp=I.MOVL, src=srcLoc, dst=dstLoc}]
          end

      fun spillInit () = 
        (firstSpill := true;
         memRegsUsed := 0w0;
         Intmap.clear affectedBlocks; 
         Intmap.clear deadRegs;
         X86StackSpills.init(); FR.reset(); GR8.reset())

      (* Dedicated + available registers *)
      fun mark(a, l) = app (fn r => Array.update(a, r, true)) l

      val dedicatedR = Array.array(32,false)
      val dedicatedF = Array.array(64,false)
      val _ = mark(dedicatedR, R.dedicatedR)
      val _ = mark(dedicatedF, R.dedicatedF)
 
      fun ra(cluster as F.CLUSTER{regmap, ...}) = 
      let val printGraph = 
              if !x86CfgDebugFlg then 
                 PrintFlowGraph.printCluster(!CG.printFlowgraphStream)
              else fn msg => fn _ => () 
  
          val _ = spillInit()
  
          val (n,m) = X86RewritePseudo.rewrite 
                       {firstPseudo=32, 
                        originalRegmap=I.C.lookup regmap, 
                        pruneCellSets=not targetMemoryRegisters 
                       } cluster

          (* generic register allocator *)

          val cluster = Ra.ra
                         [ {spill     = spillR8,
                            spillSrc  = spillReg,
                            spillCopyTmp= spillCopyTmp,
                            reload    = reloadR8,
                            reloadDst = reloadReg,
                            renameSrc = renameR8,
                            copyInstr = copyInstrR,
                            K         = K8,
                            getreg    = GR8.getreg,
                            cellkind  = I.C.GP,   
                            dedicated = dedicatedR,
                            spillProh = [(n,m)],
                            firstMemReg= 8,
                            numMemRegs= if targetMemoryRegisters then 24 else 0,
                            mode      = Ra.SPILL_PROPAGATION+Ra.SPILL_COLORING
                           },
                           {spill     = spillF,
                            spillSrc  = spillFreg,
                            spillCopyTmp= spillFcopyTmp,
                            reload    = reloadF,
                            reloadDst = reloadFreg,
                            renameSrc = renameF,
                            copyInstr = copyInstrF,
                            K         = KF,
                            getreg    = FR.getreg,
                            cellkind  = I.C.FP,   
                            dedicated = dedicatedF,
                            spillProh = [],
                            firstMemReg=0,
                            numMemRegs= 0,
                            mode      = Ra.SPILL_PROPAGATION
                           }] cluster
          val _ = removeDeadCode cluster
          val _ = printGraph "\t---After register allocation K=8---\n" cluster
      in  cluster
      end

    end (* RegAllocation *)

  ) 

