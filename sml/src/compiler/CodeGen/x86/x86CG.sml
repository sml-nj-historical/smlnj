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
    structure InsnProps  = X86Props
    structure Asm        = X86AsmEmitter
    structure Shuffle    = X86Shuffle

    val spill = CPSRegions.spill 
    val stack = CPSRegions.stack 
    val esp   = I.C.esp

    fun error msg = MLRiscErrorMsg.error("X86CG",msg)

    val fast_floating_point = MLRiscControl.getFlag "x86-fast-fp"

    structure MLTreeComp=
       X86(structure X86Instr=X86Instr
           structure X86MLTree=X86MLTree
           structure ExtensionComp = X86MLTreeExtComp
               (structure I = X86Instr
                structure T = X86MLTree
               )
           val tempMem = I.Displace{base=esp, disp=I.Immed 304, mem=stack}
           fun cvti2f{src,ty} = (* ty is always 32 for SML/NJ *)
               {instrs  = [I.MOVE{mvOp=I.MOVL, src=src, dst=tempMem}],
                tempMem = tempMem, 
                cleanup = []
               }
           datatype arch = Pentium | PentiumPro | PentiumII | PentiumIII
           val arch = ref Pentium (* Lowest common denominator *)
           val fast_floating_point = fast_floating_point
          )

    structure Jumps = 
       X86Jumps(structure Instr=X86Instr
                structure AsmEmitter=X86AsmEmitter
                structure Shuffle=X86Shuffle
                structure MCEmitter=X86MCEmitter)
   
    structure BackPatch = 
       BackPatch(structure Jumps=Jumps
                 structure Emitter=X86MCEmitter
                 structure Props=InsnProps
                 structure Flowgraph=X86FlowGraph
                 structure Asm=X86AsmEmitter
                 structure CodeString=CodeString)

    structure PrintFlowGraph=
       PrintCluster(structure Flowgraph=X86FlowGraph
                    structure Asm = X86AsmEmitter)

    structure X86FP = 
       X86FP(structure X86Instr = X86Instr
             structure X86Props = InsnProps
             structure Flowgraph = X86FlowGraph
             structure Liveness = Liveness(X86FlowGraph)
             structure Asm = X86AsmEmitter
            )

    structure X86Spill = X86Spill(structure Instr=I structure Props=InsnProps)

    val toInt32 = Int32.fromInt
    fun cacheOffset r = I.Immed(toInt32(X86Runtime.vregStart + 
                                Word.toIntX(Word.<<(Word.fromInt(r-8),0w2))))
    fun cacheFPOffset f = I.Immed(toInt32(X86Runtime.vFpStart + 
                                Word.toIntX(Word.<<(Word.fromInt(f-40),0w3))))


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
      val affectedBlocks =
	  IntHashTable.mkTable(32,X86DeadCode) : bool IntHashTable.hash_table
      val deadRegs       =
	  IntHashTable.mkTable(32,X86DeadCode) : bool IntHashTable.hash_table
      fun removeDeadCode(F.CLUSTER{blocks, ...}) =
      let fun isDead i = getOpt (IntHashTable.find deadRegs i, false)
          fun isAffected i = getOpt (IntHashTable.find affectedBlocks i, false)
          fun isDeadInstr(I.ANNOTATION{i, ...}) = isDeadInstr i 
            | isDeadInstr(I.MOVE{dst=I.Direct rd, ...}) = isDead rd
            | isDeadInstr(I.MOVE{dst=I.MemReg rd, ...}) = isDead rd
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
      in if IntHashTable.numItems affectedBlocks > 0 then 
            (scan blocks;
	     IntHashTable.clear deadRegs;
	     IntHashTable.clear affectedBlocks)
         else ()
      end

      (* This function finds out which pseudo memory registers are unused.
       * Those that are unused are made available for spilling.
       * The register allocator calls this function right before spilling 
       * a set of nodes.
       *)
      local 
         open RAGraph
      in  
         val firstSpill = ref true
         val firstFPSpill = ref true
         fun spillInit(GRAPH{nodes, ...}, I.C.GP) = 
             if !firstSpill then (* only do this once! *)
             let val lookup = IntHashTable.lookup nodes
                 fun find(r, free) =
                     if r >= 10 then (* note, %8 and %9 are reserved! *)
                        let val free = 
                                case lookup r of
                                  NODE{uses=ref [], defs=ref [], ...} => 
                                     cacheOffset r::free
                                | _ => free
                        in  find(r-1, free) end
                     else 
                        free
                 val free = find(31 (* X86Runtime.numVregs+8-1 *), [])
              in firstSpill := false;
                 X86StackSpills.setAvailableOffsets free
              end 
              else ()
            | spillInit(GRAPH{nodes, ...}, I.C.FP) = 
              if !firstFPSpill andalso !fast_floating_point then
              let val lookup = IntHashTable.lookup nodes
                 fun find(r, free) =
                     if r >= 32+8 then 
                        let val free = 
                                case lookup r of
                                  NODE{uses=ref [], defs=ref [], ...} => 
                                     cacheFPOffset r::free
                                | _ => free
                        in  find(r-1, free) end
                     else 
                        free
                 val free = find(63, [])
              in firstFPSpill := false;
                 X86StackSpills.setAvailableFPOffsets free
              end 
              else ()
      end
 
      (* This is the generic register allocator *)
      structure Ra = 
        RegisterAllocator
         (ChowHennessySpillHeur)
         (MemoryRA             (* for memory coalescing *)
           (RADeadCodeElim     (* do the funky dead code elimination stuff *)
              (ClusterRA
                 (structure Flowgraph = F
                  structure Asm = X86AsmEmitter
                  structure InsnProps = InsnProps
                  structure Spill = RASpill
                     (structure Asm = X86AsmEmitter
                      structure InsnProps = InsnProps
                     )
                 )
              )
              (fun cellkind I.C.GP = true | cellkind _ = false
               val deadRegs = deadRegs
               val affectedBlocks = affectedBlocks
               val spillInit = spillInit
              )
           )
        )


      (* -------------------------------------------------------------------
       * Floating point stuff 
       * -------------------------------------------------------------------*)
      val availF8 = map I.C.FPReg [0,1,2,3,4,5,6]
      val KF32 = length R.availF
      structure FR32 = GetReg(val nRegs=KF32 
                              val available=R.availF 
                              val first=I.C.ST 8)

      val availF8 = map I.C.FPReg [0,1,2,3,4,5,6] 
      val KF8  = length availF8
      structure FR8  = GetReg(val nRegs=KF8
                              val available=availF8
                              val first=I.C.ST 0)
 
 
      (* -------------------------------------------------------------------
       * Callbacks for floating point K=32 
       * -------------------------------------------------------------------*)
      fun copyInstrF((rds as [_], rss as [_]), _) =
            [I.FCOPY{dst=rds, src=rss, tmp=NONE}]
        | copyInstrF((rds, rss), I.FCOPY{tmp, ...}) = 
            [I.FCOPY{dst=rds, src=rss, tmp=tmp}]


      fun getFregLoc loc = 
          if loc >= 0 then I.FDirect loc
          else 
            I.Displace{base=esp, disp=X86StackSpills.getFregLoc loc, mem=spill}
  
      (* spill floating point *)
      fun spillF{instr, reg, spillLoc, kill, regmap, annotations} = 
          (floatSpillCnt := !floatSpillCnt + 1;
           X86Spill.fspill(instr, regmap, reg, getFregLoc spillLoc) 
          )
  
      fun spillFreg{src, reg, spillLoc, annotations} = 
         (floatSpillCnt := !floatSpillCnt + 1;
          [I.FLDL(I.FDirect(src)), I.FSTPL(getFregLoc spillLoc)]
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
           [I.FLDL(getFregLoc spillLoc), I.FSTPL(I.FDirect dst)]
          )

      (* -------------------------------------------------------------------
       * Callbacks for floating point K=7 
       * -------------------------------------------------------------------*)
      fun FMemReg f = if f >= 32+8 andalso f < 32+32 
                       then I.FDirect f else I.FPR f

      fun copyInstrF'((rds as [d], rss as [s]), _) =
           [I.FMOVE{fsize=I.FP64,src=FMemReg s,dst=FMemReg d}]
        | copyInstrF'((rds, rss), I.FCOPY{tmp, ...}) = 
           [I.FCOPY{dst=rds, src=rss, tmp=tmp}]

      fun spillFreg'{src, reg, spillLoc, annotations} = 
          (floatSpillCnt := !floatSpillCnt + 1;
           [I.FMOVE{fsize=I.FP64, src=FMemReg src, dst=getFregLoc spillLoc}]
          )

      fun renameF'{instr, fromSrc, toSrc, regmap} =
          (floatRenameCnt := !floatRenameCnt + 1;
           X86Spill.freload(instr, regmap, fromSrc, I.FPR toSrc)
          )

      fun reloadFreg'{dst, reg, spillLoc, annotations} = 
          (floatReloadCnt := !floatReloadCnt + 1;
           [I.FMOVE{fsize=I.FP64, dst=FMemReg dst, src=getFregLoc spillLoc}]
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

      fun getRegLoc(spillLoc) = 
               (* Is it a memory register? *)
          if spillLoc >= 0 then I.MemReg spillLoc 
               (* No, logical spill locations... *)
          else I.Displace{base=esp, disp=getRegLoc' spillLoc, mem=spill}
          
  
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
         firstFPSpill := true;
         IntHashTable.clear affectedBlocks; 
         IntHashTable.clear deadRegs;
         X86StackSpills.init(); 
         if !fast_floating_point then FR8.reset() else FR32.reset(); 
         GR8.reset())

      (* Dedicated + available registers *)
      fun mark(a, l) = app (fn r => Array.update(a, r, true)) l

      val dedicatedR   = Array.array(32,false)
      val dedicatedF32 = Array.array(64,false)
      val dedicatedF8  = Array.array(64,false)
      val _ = mark(dedicatedR, R.dedicatedR)
      val _ = mark(dedicatedF32, R.dedicatedF)

      (* RA parameters *)

      (* How to allocate integer registers:    
       * Perform register alocation + memory allocation
       *)
      val RAInt = {spill     = spillR8,
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
                   spillProh = [],
                   memRegs   = [(8,31)],
                   mode      = Ra.SPILL_PROPAGATION+Ra.SPILL_COLORING
                  } : Ra.raClient

      (* How to allocate floating point registers:    
       * Allocate all fp registers on the stack.  This is the easy way.
       *)
      val RAFP32 ={spill     = spillF,
                   spillSrc  = spillFreg,
                   spillCopyTmp= spillFcopyTmp,
                   reload    = reloadF,
                   reloadDst = reloadFreg,
                   renameSrc = renameF,
                   copyInstr = copyInstrF,
                   K         = KF32,
                   getreg    = FR32.getreg,
                   cellkind  = I.C.FP,   
                   dedicated = dedicatedF32,
                   spillProh = [],
                   memRegs   = [],
                   mode      = Ra.SPILL_PROPAGATION
                  } : Ra.raClient

      (* How to allocate floating point registers:    
       * Allocate fp registers on the %st stack.  Also perform
       * memory allcoation.
       *)
       val RAFP8 ={spill     = spillF,
                   spillSrc  = spillFreg',
                   spillCopyTmp= spillFcopyTmp,
                   reload    = reloadF,
                   reloadDst = reloadFreg',
                   renameSrc = renameF',
                   copyInstr = copyInstrF',
                   K         = KF8,
                   getreg    = FR8.getreg,
                   cellkind  = I.C.FP,   
                   dedicated = dedicatedF8,
                   spillProh = [],
                   memRegs   = [(I.C.FPReg 8,I.C.FPReg 31)],
                   mode      = Ra.SPILL_PROPAGATION+Ra.SPILL_COLORING
                  } : Ra.raClient

      (* Two RA modes, fast and normal *) 
      val fast_fp = [RAInt, RAFP8]
      val normal_fp = [RAInt, RAFP32]
 
      (* The main ra routine *)
      fun ra(cluster as F.CLUSTER{regmap, ...}) = 
      let val printGraph = 
              if !x86CfgDebugFlg then 
                 PrintFlowGraph.printCluster(!CG.printFlowgraphStream)
              else fn msg => fn _ => () 
  
          val _ = spillInit()
  
          (* generic register allocator *)

          val cluster = Ra.ra
                        (if !fast_floating_point then fast_fp else normal_fp)
                        cluster
          val _ = removeDeadCode cluster
          val _ = printGraph "\t---After register allocation K=8---\n" cluster

          (* Run the FP translation phase when fast floating point has
           * been enabled
           *)
          val cluster = 
               if !fast_floating_point andalso I.C.numCell I.C.FP () > 0 then 
               let val cluster = X86FP.run cluster
               in  printGraph "\t---After X86 FP translation ---\n" cluster;
                   cluster
               end
               else cluster
      in  cluster
      end

    end (* RegAllocation *)

  ) 

