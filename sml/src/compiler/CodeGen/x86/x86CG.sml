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
    structure InsnProps  = X86Props(X86Instr)
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
    fun cacheOffset r = I.Immed(toInt32(X86Runtime.vregStart + (r-8)*4))

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
  
     (* create integer and floating point register allocators *)
      structure X86Ra = 
         X86RegAlloc(structure P = InsnProps
                     structure I = X86Instr
                     structure F = X86FlowGraph
                     structure Asm = X86AsmEmitter)
  
      structure FR = GetReg(val nRegs=32 
                            val available=R.availF 
                            val first=32)
  
      structure FloatRaUser : RA_USER_PARAMS = struct
        structure I = I
        structure B = F.B
  
        val nFreeRegs = length R.availF
        val dedicated = R.dedicatedF
        fun copyInstr((rds as [_], rss as [_]), _) =
            I.FCOPY{dst=rds, src=rss, tmp=NONE}
          | copyInstr((rds, rss), I.FCOPY{tmp, ...}) = 
            I.FCOPY{dst=rds, src=rss, tmp=tmp}
  
        val getreg = FR.getreg
        val getFregLoc = X86StackSpills.getFregLoc
  
       (* register allocation for floating point registers *)
        fun spill{instr, reg, regmap, id} = let
          (* val _ = floatSpillCnt := !floatSpillCnt + 1 *)
          val slot = I.Displace{base=esp, disp=getFregLoc reg, mem=stack}
          fun spillInstr(r) = [I.FLD(I.FDirect(r)), I.FSTP(slot)]
        in
          case instr
          of I.FCOPY{tmp, dst, src, ...} => let
               fun spillCpy([fd], [fs]) = {code=spillInstr(fs), proh=[], instr=NONE}
             in
               case tmp
               of SOME(I.Direct r) =>
                  if regmap r=reg then let 
                      val slot = I.Displace{base=esp, disp=getFregLoc reg,
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
  
        fun reload{instr, reg, regmap, id} = let
          (* val _ = floatReloadCnt := !floatReloadCnt + 1 *)
          val slot = I.Displace{base=esp, disp=getFregLoc reg, mem=stack}
          fun reloadInstr(r, rest) = I.FLD(slot) :: I.FSTP(I.FDirect r) :: rest
        in
          case instr
          of I.FCOPY{dst=[rd], src=[rs], ...} =>
               {code=reloadInstr(rd, []),
                proh=[]}
           | _ => X86Spill.freload(instr, reg, slot)
          (*esac*)
        end
      end (* FloatRaUser *)
  
      structure FloatRa = X86Ra.FloatRa(structure RaUser=FloatRaUser)
  
      local 
        fun range(_, 0) = []
          | range(r, n) = r::range(r+1, n-1)
      in
        (* Should experiment with high numVregs here. *)
        val availR32 = X86CpsRegs.availR @ range(8, X86Runtime.numVregs)
      end
  
      structure GR32 = GetReg(val nRegs=32  val available=availR32
                              val first=0)
  
      structure IntRa32User : RA_USER_PARAMS = struct
        structure I = I
        structure B = F.B
  
        val nFreeRegs = length (availR32)
        val dedicated = X86CpsRegs.dedicatedR
        fun copyInstr((rds as [_], rss as [_]), _) =
            I.COPY{dst=rds, src=rss, tmp=NONE}
          | copyInstr((rds, rss), I.COPY{tmp, ...}) = 
            I.COPY{dst=rds, src=rss, tmp=tmp}
  
        (* avoid the physical registers when possible. *)
        fun getreg{pref, proh, stamp} = 
        let fun add([],trail) = trail
              | add(r::rs,trail) = 
                 let val x = Array.sub(proh,r)
                 in  Array.update(proh,r,stamp); 
                     add(rs,if x <> stamp then r::trail else trail) 
                 end
            fun reset(r::rs) = (Array.update(proh,r,~1); reset(rs))
              | reset [] = ()
            val trail = add(X86CpsRegs.availR,[])
        in  GR32.getreg{pref=pref, stamp=stamp, proh=proh}
               handle _ => (reset trail; 
                            GR32.getreg{pref=pref, stamp=stamp, proh=proh})
        end
  
        fun getreg{pref, proh, stamp} = 
             GR32.getreg{pref=pref, proh=proh, stamp=stamp} (* XXX *)
  
        val getRegLoc = X86StackSpills.getRegLoc
  
        fun spill{instr, reg, regmap, id} = let
          (* val _ = intSpillCnt := !intSpillCnt + 1 *)
          val slot = I.Displace{base=esp, disp=getRegLoc reg, mem=stack}
          fun spillInstr(r) =
            [I.MOVE{mvOp=I.MOVL, src=I.Direct r, dst=slot}]
        in
          case instr
          of I.COPY{tmp, dst, src, ...} => let
              fun spillCpy([rd], [rs]) = {code=spillInstr(rs), proh=[], instr=NONE}
              in
               case tmp
               of SOME(I.Direct r) =>
                   if regmap r=reg then
                     {code=[], proh=[],
                      instr=
                       SOME(I.COPY
                            {dst=dst, src=src,
                             tmp=SOME(I.Displace{base=esp, disp=getRegLoc r,
                                                 mem=stack})})}
                   else
                     spillCpy(dst, src) 
                | _ => spillCpy(dst, src)
             end
           | _ => X86Spill.spill(instr, reg, slot)
        end
  
        fun reload{instr, reg, regmap, id} = let
          (* val _ = intReloadCnt := !intReloadCnt + 1 *)
          val slot = I.Displace{base=esp, disp=getRegLoc reg, mem=stack}
          fun reloadInstr(r, rest) =
            I.MOVE{mvOp=I.MOVL, src=slot, dst=I.Direct r}::rest
        in
          case instr
          of I.COPY{dst=[rd],src=[rs], ...} =>
               {code=reloadInstr(rd, []),
                proh=[]}
           | _ => X86Spill.reload(instr, reg, slot)
        end
      end
  
      structure IntRA32 = X86Ra.IntRa(structure RaUser= IntRa32User)
 
      fun noSpillSlotTbl _ = error "No spillSlotTbl"
  
      val spillSlotTbl : (int -> int) ref = ref noSpillSlotTbl
  
      structure GR8 = GetReg(val nRegs=8 val available=X86CpsRegs.availR
                             val first=0)
   
      structure IntRa8User : RA_USER_PARAMS = struct
        structure I = I
        structure B = F.B
  
        val nFreeRegs = length X86CpsRegs.availR
        val dedicated = R.dedicatedR
        fun copyInstr((rds as [_], rss as [_]), _) =
                I.COPY{dst=rds, src=rss, tmp=NONE}
          | copyInstr((rds, rss), I.COPY{tmp, ...}) = 
                I.COPY{dst=rds, src=rss, tmp=tmp}
  
        val getreg = GR8.getreg
  
        fun getRegLoc reg = let
          val recommended = ! spillSlotTbl reg
        in 
          if recommended < 32 then cacheOffset recommended
          else error ("getRegLoc:RA8 " ^ Int.toString recommended ^ "\n")
        end  
  
       (* register allocation for general purpose registers *)
        fun spill{instr, reg, regmap, id} = let
          (* val _ = intSpillCnt := !intSpillCnt + 1 *)
          val slot = I.Displace{base=esp, disp=getRegLoc reg, mem=stack}
          fun spillInstr(r) =
            [I.MOVE{mvOp=I.MOVL, src=I.Direct r, dst=slot}]
        in
          case instr
          of I.COPY{tmp, dst, src, ...} => let
              fun spillCpy([rd], [rs]) = {code=spillInstr(rs), proh=[], 
                                          instr=NONE}
              in
               case tmp
               of SOME(I.Direct r) =>
                   if regmap r=reg then
                     {code=[], proh=[], 
                      instr=
                       SOME(I.COPY
                         {dst=dst, src=src, 
                          tmp=SOME(I.Displace{base=esp, disp=getRegLoc r,
                                              mem=stack})})}
                   else
                     spillCpy(dst, src)
                | _ => spillCpy(dst, src)
             end
           | _ => X86Spill.spill(instr, reg, slot)
        end handle X86Cells.Cells =>
          (print ("in spill handler "^ Int.toString reg ^ "\n");
           {code=[instr], proh=[reg], instr=NONE})
  
        fun reload{instr, reg, regmap, id} = let
          (* val _ = intReloadCnt := !intReloadCnt + 1 *)
          val slot = I.Displace{base=esp, disp=getRegLoc reg, mem=stack}
          fun reloadInstr(r, rest) =
            I.MOVE{mvOp=I.MOVL, src=slot, dst=I.Direct r}::rest
        in
          case instr
          of I.COPY{dst=[rd],src=[rs], ...} =>
               {code=reloadInstr(rd, []),
                proh=[]}
           | _ => X86Spill.reload(instr, reg, slot)
        end handle X86Cells.Cells =>
          (print ("in reload handler " ^ Int.toString reg ^ "\n");
           {code=[instr], proh=[reg]})
      end (* IntRa8User *)
  
      structure IntRA8 = X86Ra.IntRa(structure RaUser= IntRa8User)
  
      fun spillInit () = 
        (* X86StackSpills is esential; 
         * the rest is just to ensure repeatability between compilation runs.
         *)
        (spillSlotTbl := noSpillSlotTbl;
         X86StackSpills.init(); GR32.reset(); FR.reset(); GR8.reset())
  
      fun ra(cluster as F.CLUSTER{regmap, ...}) = let
        fun rmPseudoPhysical(rmap, n) =
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
  
        fun intra32 cluster = let
          val ra = IntRA32.ra IntRA32.REGISTER_ALLOCATION [] 
          val cluster' as F.CLUSTER{regmap, ...} = ra cluster 
        in spillSlotTbl := I.C.lookup regmap; cluster'
        end
  
        fun insertPseudoPhysical(F.CLUSTER{regmap, ...}) = let
          val addIt = Intmap.add regmap
          fun add(32) = ()
            | add(n) = (addIt (n, n); add(n+1))
        in add(8)
        end
  
        fun preference r = let
          val pref = ! spillSlotTbl r
        in if pref >= 0 andalso pref < 8 then SOME pref else NONE
        end handle _ => NONE
  
        val intra8 = IntRA8.ra IntRA8.REGISTER_ALLOCATION 
        val floatRa = FloatRa.ra FloatRa.REGISTER_ALLOCATION [] 
        val clonedRmap = cloneRegmap regmap
   
        val printGraph = 
          if !x86CfgDebugFlg then 
            PrintFlowGraph.printCluster(!CG.printFlowgraphStream)
          else fn msg => fn _ => () 
  
        val _ = spillInit()
  
        val _ = printGraph "\t---Before register allocation---\n" cluster
  
        val cluster = setRegMap clonedRmap cluster
        val cluster = intra32 cluster
        val _ = printGraph "\t---After register allocation K=32---\n" cluster 
  
        val (n,m) = X86RewritePseudo.rewrite 32 (I.C.lookup regmap) cluster
  
        val cluster = setRegMap regmap cluster
        val _ =         rmPseudoPhysical(regmap, 8)
        val cluster = intra8 [(n,m)] (* preference *) cluster 
        val _ = printGraph "\t---After register allocation K=8---\n" cluster
  
        val _ = insertPseudoPhysical cluster
        val cluster = floatRa cluster
        val _ = printGraph "\t---After floating register allocation---\n" 
                    cluster
      in 
        (*
         spillInit(); 
         (floatRa o intra8 o (setRegMap clonedRmap) o intra32) cluster
         *)
        cluster
      end

      fun cp _ = error "copy propagation"
    end (* RegAllocation *)

  ) 

