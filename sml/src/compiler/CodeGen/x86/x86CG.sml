(*
 * X86 specific backend
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

    val stack = I.Region.stack 
    val esp   = I.C.esp

    fun error msg = MLRiscErrorMsg.error("X86CG",msg)

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

    structure X86Spill = X86Spill(structure Instr=I structure Asm=Asm)

    val toInt32 = Int32.fromInt
    fun cacheOffset r = I.Immed(toInt32(X86Runtime.vregStart + 
                                Word.toIntX(Word.<<(Word.fromInt(r-8),0w2))))

    structure X86RewritePseudo=
       X86RewritePseudo(structure Instr=X86Instr
                        structure Flowgraph=X86FlowGraph
                        structure Shuffle=X86Shuffle
                        fun ea r = I.Displace{base=esp, disp=cacheOffset r,
                                              mem=stack}
                       )


    val intSpillCnt = MLRiscControl.getCounter "ra-int-spills"
    val floatSpillCnt = MLRiscControl.getCounter "ra-float-spills"
    val intReloadCnt = MLRiscControl.getCounter "ra-int-reloads"
    val floatReloadCnt = MLRiscControl.getCounter "ra-float-reloads"
    val x86CfgDebugFlg = MLRiscControl.getFlag "x86-cfg-debug"

    structure RA = 
    struct
      structure F = F

      (* This is the generic register allocator *)
      structure Ra = 
        RegisterAllocator
         (ChaitinSpillHeur)
         (ClusterRA
            (structure Flowgraph = F
             structure Asm = X86AsmEmitter
             structure InsnProps = InsnProps
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
            I.FCOPY{dst=rds, src=rss, tmp=NONE}
        | copyInstrF((rds, rss), I.FCOPY{tmp, ...}) = 
            I.FCOPY{dst=rds, src=rss, tmp=tmp}
  
      val getFregLoc = X86StackSpills.getFregLoc
  
      (* spill floating point *)
      fun spillF{instr, reg, spillLoc, graph, kill, regmap, annotations} = let
          val _ = floatSpillCnt := !floatSpillCnt + 1
          val slot = I.Displace{base=esp, disp=getFregLoc spillLoc, mem=stack}
          fun spillInstr(r) = [I.FLD(I.FDirect(r)), I.FSTP(slot)]
        in
          case instr
          of I.FCOPY{tmp, dst, src, ...} => let
               fun spillCpy([fd], [fs]) = {code=spillInstr(fs), proh=[], 
                                           instr=NONE}
             in
               case tmp
               of SOME(I.Direct r) =>
                  if r=reg then let 
                      val slot = I.Displace{base=esp, disp=getFregLoc spillLoc,
                                            mem=stack}
                      val fcopy = I.FCOPY{dst=dst, src=src, tmp=SOME slot}
                    in  {code=[],  proh=[], instr= SOME fcopy}
                    end
                   else
                     spillCpy(dst, src)
                | _ => spillCpy(dst, src)
             end
           | _ => X86Spill.fspill(instr, reg, slot)
          (*esac*)
      end
  
      (* reload floating point *)
      fun reloadF{instr, reg, spillLoc, graph, regmap, annotations} = 
      let val _ = floatReloadCnt := !floatReloadCnt + 1
          val slot = I.Displace{base=esp, disp=getFregLoc spillLoc, mem=stack}
          fun reloadInstr(r, rest) = I.FLD(slot) :: I.FSTP(I.FDirect r) :: rest
      in  case instr
          of I.FCOPY{dst=[rd], src=[rs], ...} =>
               {code=reloadInstr(rd, []), proh=[]}
           | _ => X86Spill.freload(instr, reg, slot)
          (*esac*)
      end

      (* -------------------------------------------------------------------
       * Integer 32 stuff 
       * -------------------------------------------------------------------*)
      local 
        fun range(_, 0) = []
          | range(r, n) = r::range(r+1, n-1)
      in
        (* Should experiment with high numVregs here. *)
        val availR32 = X86CpsRegs.availR @ range(8, X86Runtime.numVregs)
      end
  
      structure GR32 = GetReg(val nRegs=32  val available=availR32
                              val first=0)
  
      val K32 = length (availR32)

      fun copyInstrR((rds as [_], rss as [_]), _) =
           I.COPY{dst=rds, src=rss, tmp=NONE}
        | copyInstrR((rds, rss), I.COPY{tmp, ...}) = 
           I.COPY{dst=rds, src=rss, tmp=tmp}
  
      val getRegLoc = X86StackSpills.getRegLoc

      fun spillR32{instr, reg, spillLoc, graph, kill, regmap, annotations} = 
          let val _ = intSpillCnt := !intSpillCnt + 1
              val slot = I.Displace{base=esp, disp=getRegLoc spillLoc,mem=stack}
              fun spillInstr(r) =
                [I.MOVE{mvOp=I.MOVL, src=I.Direct r, dst=slot}]
          in
             case instr
             of I.COPY{tmp, dst, src, ...} => 
                let fun spillCpy([rd], [rs]) = 
                          {code=spillInstr(rs), proh=[], instr=NONE}
                in case tmp of 
                     SOME(I.Direct r) =>
                     if r=reg then
                     {code=[], proh=[],
                      instr=
                       SOME(I.COPY
                            {dst=dst, src=src,
                             tmp=SOME(I.Displace{base=esp, 
                                                 disp=getRegLoc spillLoc,
                                                 mem=stack})})}
                   else
                     spillCpy(dst, src) 
                | _ => spillCpy(dst, src)
                end
              | _ => X86Spill.spill(instr, reg, slot)
          end
  
      fun reloadR32{instr, reg, spillLoc, graph, regmap, annotations} = 
      let val _ = intReloadCnt := !intReloadCnt + 1 
          val slot = I.Displace{base=esp, disp=getRegLoc spillLoc, mem=stack}
          fun reloadInstr(r, rest) =
            I.MOVE{mvOp=I.MOVL, src=slot, dst=I.Direct r}::rest
      in  case instr
          of I.COPY{dst=[rd],src=[rs], ...} =>
               {code=reloadInstr(rd, []),
                proh=[]}
           | _ => X86Spill.reload(instr, reg, slot)
      end
  
      (* -------------------------------------------------------------------
       * Integer 8 stuff 
       * -------------------------------------------------------------------*)
      fun noSpillSlotTbl _ = error "No spillSlotTbl"
  
      val spillSlotTbl : (int -> int) ref = ref noSpillSlotTbl
  
      structure GR8 = GetReg(val nRegs=8 val available=X86CpsRegs.availR
                             val first=0)
   
      val K8 = length X86CpsRegs.availR
  
      fun getRegLoc reg = 
      let val recommended = ! spillSlotTbl reg
      in  if recommended < 32 then cacheOffset recommended
          else error ("getRegLoc:RA8 " ^ Int.toString recommended ^ "\n")
      end  
  
       (* register allocation for general purpose registers *)
      fun spillR8{instr, reg, spillLoc, graph, kill, regmap, annotations} = 
          let val _ = intSpillCnt := !intSpillCnt + 1
              val slot = I.Displace{base=esp, disp=getRegLoc spillLoc,mem=stack}
              fun spillInstr(r) =
                [I.MOVE{mvOp=I.MOVL, src=I.Direct r, dst=slot}]
          in case instr
             of I.COPY{tmp, dst, src, ...} => 
                let fun spillCpy([rd], [rs]) = {code=spillInstr(rs), proh=[], 
                                                instr=NONE}
                in case tmp
                   of SOME(I.Direct r) =>
                      if r=reg then
                      {code=[], proh=[], 
                       instr=
                       SOME(I.COPY
                         {dst=dst, src=src, 
                          tmp=SOME(I.Displace{base=esp, disp=getRegLoc spillLoc,
                                              mem=stack})})}
                      else
                        spillCpy(dst, src)
                   | _ => spillCpy(dst, src)
                 end
           | _ => X86Spill.spill(instr, reg, slot)
      end handle X86Cells.Cells =>
          (print ("in spill handler "^ Int.toString reg ^ "\n");
           {code=[instr], proh=[reg], instr=NONE})
  
      fun reloadR8{instr, reg, spillLoc, graph, regmap, annotations} = 
      let val _ = intReloadCnt := !intReloadCnt + 1
          val slot = I.Displace{base=esp, disp=getRegLoc spillLoc, mem=stack}
          fun reloadInstr(r, rest) =
            I.MOVE{mvOp=I.MOVL, src=slot, dst=I.Direct r}::rest
      in  case instr
          of I.COPY{dst=[rd],src=[rs], ...} =>
               {code=reloadInstr(rd, []),
                proh=[]}
           | _ => X86Spill.reload(instr, reg, slot)
      end handle X86Cells.Cells =>
          (print ("in reload handler " ^ Int.toString reg ^ "\n");
           {code=[instr], proh=[reg]})

      fun spillInit () = 
        (* X86StackSpills is esential; 
         * the rest is just to ensure repeatability between compilation runs.
         *)
        (spillSlotTbl := noSpillSlotTbl;
         X86StackSpills.init(); GR32.reset(); FR.reset(); GR8.reset())

      (* Dedicated + available registers *)
      fun mark(a, l) = app (fn r => Array.update(a, r, true)) l

      val dedicatedR = Array.array(32,false)
      val dedicatedF = Array.array(64,false)
      val _ = mark(dedicatedR, R.dedicatedR)
      val _ = mark(dedicatedF, R.dedicatedF)
 
      fun ra(cluster as F.CLUSTER{regmap, ...}) = 
      let fun rmPseudoPhysical(rmap, n) =
          let val rmv = Intmap.rmv rmap
              fun loop 32 = () 
                | loop n  = (rmv n; loop(n+1))
          in  loop n end
  
          fun cloneRegmap regmap = Intmap.copy regmap
  
          fun setRegMap rmap 
              (F.CLUSTER{blocks, entry, exit, blkCounter, annotations, ...}) =
              F.CLUSTER{blocks=blocks, 
                        entry=entry, 
                        exit=exit, 
                        blkCounter=blkCounter,
                        regmap=rmap,
                        annotations=annotations}

          fun intra32 cluster = 
          let val cluster' as F.CLUSTER{regmap, ...} = 
                  Ra.ra Ra.REGISTER_ALLOCATION 
                    [{spill       = spillR32,
                      reload      = reloadR32,
                      copyInstr   = copyInstrR,
                      K           = K32,
                      getreg      = GR32.getreg,
                      cellkind    = I.C.GP,   
                      dedicated   = dedicatedR,
                      spillProh   = [],
                      optimizations=[Ra.DEAD_COPY_ELIM]
                     }] cluster
          in spillSlotTbl := I.C.lookup regmap; cluster'
          end
  
          fun insertPseudoPhysical(F.CLUSTER{regmap, ...}) = 
          let val addIt = Intmap.add regmap
              fun add(32) = ()
                | add(n) = (addIt (n, n); add(n+1))
          in add(8)
          end
  
          fun preference r = 
          let val pref = ! spillSlotTbl r
          in if pref >= 0 andalso pref < 8 then SOME pref else NONE
          end handle _ => NONE
  
          val clonedRmap = cloneRegmap regmap
   
          val printGraph = 
              if !x86CfgDebugFlg then 
                 PrintFlowGraph.printCluster(!CG.printFlowgraphStream)
              else fn msg => fn _ => () 
  
          val _ = spillInit()
  
          val _ = printGraph "\t---Before register allocation---\n" cluster
 
          (* Integer 32 allocation *) 
          val cluster = setRegMap clonedRmap cluster
          val cluster = intra32 cluster
          val _ = printGraph "\t---After register allocation K=32---\n" cluster 
  
          (* Rewrite the flowgraph *)
          val cluster = setRegMap clonedRmap cluster
          val (n,m) = X86RewritePseudo.rewrite 32 (I.C.lookup regmap) cluster
  
          val cluster = setRegMap regmap cluster
          val _       = rmPseudoPhysical(regmap, 8)

          (* generic register allocator *)
          val ra = Ra.ra Ra.REGISTER_ALLOCATION
                         [ {spill     = spillR8,
                            reload    = reloadR8,
                            copyInstr = copyInstrR,
                            K         = K8,
                            getreg    = GR8.getreg,
                            cellkind  = I.C.GP,   
                            dedicated = dedicatedR,
                            spillProh = [(n,m)],
                            optimizations=[]
                           },
                           {spill     = spillF,
                            reload    = reloadF,
                            copyInstr = copyInstrF,
                            K         = KF,
                            getreg    = FR.getreg,
                            cellkind  = I.C.FP,   
                            dedicated = dedicatedF,
                            spillProh = [],
                            optimizations=[]
                           }] cluster

          val _ = insertPseudoPhysical cluster

          val _ = printGraph "\t---After register allocation K=8---\n" cluster
      in  cluster
      end

      fun cp _ = error "copy propagation"
    end (* RegAllocation *)

  ) 

