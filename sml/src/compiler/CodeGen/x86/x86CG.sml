structure X86CG = struct
  structure I = X86Instr
  structure C = X86Cells
  structure R = X86CpsRegs
  structure F = X86FlowGraph
  structure Asm = X86AsmEmitter
  structure MLTree = X86MLTree
  structure B = MLTree.BNames
  structure MachSpec = X86Spec
  structure Ctrl = Control.MLRISC
  structure CG = Control.CG

  structure P = 
    X86Props(structure X86Instr=I
	     structure Shuffle=X86Shuffle)


  structure X86Spill = X86Spill(structure Instr=I structure Asm=X86AsmEmitter)

  structure PrintFlowGraph=
    PrintFlowGraphFn(structure FlowGraph=X86FlowGraph
		     structure Emitter = X86AsmEmitter)

  structure X86Jumps = 
    X86Jumps(structure Instr=X86Instr
	     structure AsmEmitter=X86AsmEmitter
	     structure Shuffle=X86Shuffle
	     structure MCEmitter=X86MCEmitter)
    
  structure BackPatch = 
    BackPatch(structure Jumps=X86Jumps
	      structure Emitter=X86MCEmitter
	      structure Props=P
	      structure Flowgraph=X86FlowGraph
	      structure Asm=X86AsmEmitter
	      structure CodeString=CodeString)

  fun error msg = ErrorMsg.impossible ("X86CG." ^ msg)
  val toInt32 = Int32.fromInt
  fun cacheOffset r = I.Immed(toInt32(X86Runtime.vregStart + (r-8)*4))

  val stack = X86Instr.Region.stack 
  val MLTree.REG stackptr = X86CpsRegs.stackptr

  structure X86RewritePseudo=
    X86RewritePseudo(structure Instr=X86Instr
		     structure Flowgraph=X86FlowGraph
		     structure Shuffle=X86Shuffle
		     fun ea r = I.Displace{base=C.esp, disp=cacheOffset r})

  val intSpillCnt = Ctrl.getInt "ra-int-spills"
  val floatSpillCnt = Ctrl.getInt "ra-float-spills"
  val intReloadCnt = Ctrl.getInt "ra-int-reloads"
  val floatReloadCnt = Ctrl.getInt "ra-float-reloads"
  val x86CfgDebugFlg = Ctrl.getFlag "x86-cfg-debug"

  structure RegAllocation : sig
     val ra : X86FlowGraph.cluster -> X86FlowGraph.cluster
    end =
  struct

   (* create integer and floating point register allocators *)
    structure X86Ra = 
       X86RegAlloc(structure P = P
		   structure I = X86Instr
		   structure F = X86FlowGraph
		   structure Asm = X86AsmEmitter)

    structure FR = GetReg(val nRegs=32 val available=R.availF)

    structure FloatRaUser : RA_USER_PARAMS = struct
      structure I = X86Instr
      structure B = B

      val nFreeRegs = length R.availF
      val dedicated = R.dedicatedF
      fun copyInstr((rds, rss), I.FCOPY{tmp, ...}) = 
	I.FCOPY{dst=rds, src=rss, tmp=tmp}

      val getreg = FR.getreg
      val getFregLoc = X86StackSpills.getFregLoc

     (* register allocation for floating point registers *)
      fun spill{instr, reg, regmap, id} = let
	val slot = I.Displace{base=stackptr, disp=getFregLoc reg}
	fun spillInstr(r) = [I.FLD(I.FDirect(r)), I.FSTP(slot)]
      in
	case instr
	of I.FCOPY{tmp, dst, src, ...} => let
	     fun spillCpy([fd], [fs]) = {code=spillInstr(fs), proh=[], instr=NONE}
	   in
	     case tmp
	     of SOME(I.Direct r) =>
	        if r=reg then let 
		    val slot = I.Displace{base=stackptr, disp=getFregLoc reg}
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
	val slot = I.Displace{base=stackptr, disp=getFregLoc reg}
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

    structure GR32 = GetReg(val nRegs=32  val available=availR32)

    structure IntRa32User : RA_USER_PARAMS = struct
      structure I = X86Instr
      structure B = B

      val nFreeRegs = length (availR32)
      val dedicated = X86CpsRegs.dedicatedR
      fun copyInstr((rds, rss), I.COPY{tmp, ...}) = 
	I.COPY{dst=rds, src=rss, tmp=tmp}

      (* avoid the physical registers when possible. *)
      fun getreg{pref, proh} = 
	GR32.getreg{pref=pref, proh=proh @ X86CpsRegs.availR}
	  handle _ => GR32.getreg{pref=pref, proh=proh}

      fun getreg{pref, proh} =  GR32.getreg{pref=pref, proh=proh} (* XXX *)

      val getRegLoc = X86StackSpills.getRegLoc

      fun spill{instr, reg, regmap, id} = let
	val slot = I.Displace{base=stackptr, disp=getRegLoc reg}
	fun spillInstr(r) =
	  [I.MOVE{mvOp=I.MOVL, src=I.Direct r, dst=slot}]
      in
	case instr
	of I.COPY{tmp, dst, src, ...} => let
	    fun spillCpy([rd], [rs]) = {code=spillInstr(rs), proh=[], instr=NONE}
 	   in
	     case tmp
	     of SOME(I.Direct r) =>
		 if r=reg then
		   {code=[], proh=[],
		    instr=
		     SOME(I.COPY
			  {dst=dst, src=src,
			   tmp=SOME(I.Displace{base=stackptr, disp=getRegLoc r})})}
		 else
		   spillCpy(dst, src) 
	      | _ => spillCpy(dst, src)
           end
	 | _ => X86Spill.spill(instr, reg, slot)
      end

      fun reload{instr, reg, regmap, id} = let
	val slot = I.Displace{base=stackptr, disp=getRegLoc reg}
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

    val spillSlotTbl : int Intmap.intmap option ref = ref NONE

    structure GR8 = GetReg(val nRegs=8 val available=X86CpsRegs.availR)
 
    structure IntRa8User : RA_USER_PARAMS = struct
      structure I = X86Instr
      structure B = B

      val nFreeRegs = length X86CpsRegs.availR
      val dedicated = R.dedicatedR
      fun copyInstr((rds, rss), I.COPY{tmp, ...}) = 
  	    I.COPY{dst=rds, src=rss, tmp=tmp}
        | copyInstr((rds, rss), instr) = let
	    val dummy : int Intmap.intmap = Intmap.new(0, Overflow)
	  in
	    X86AsmEmitter.emitInstr(instr, dummy);
	    error "copyInstr"
	  end

      val getreg = GR8.getreg

      fun getRegLoc reg = let
	val recommended = Intmap.map (Option.valOf (!spillSlotTbl)) reg
      in 
	if recommended < 32 then cacheOffset recommended
	else error ("getRegLoc:RA8 " ^ Int.toString recommended ^ "\n")
      end  

     (* register allocation for general purpose registers *)
      fun spill{instr, reg, regmap, id} = let
	val slot = I.Displace{base=stackptr, disp=getRegLoc reg}
	fun spillInstr(r) =
	  [I.MOVE{mvOp=I.MOVL, src=I.Direct r, dst=slot}]
      in
	case instr
	of I.COPY{tmp, dst, src, ...} => let
	    fun spillCpy([rd], [rs]) = {code=spillInstr(rs), proh=[], instr=NONE}
 	   in
	     case tmp
	     of SOME(I.Direct r) =>
		 if r=reg then
		   {code=[], proh=[], 
		    instr=
		     SOME(I.COPY
		       {dst=dst, src=src, 
			tmp=SOME(I.Displace{base=stackptr, disp=getRegLoc r})})}
		 else
		   spillCpy(dst, src)
	      | _ => spillCpy(dst, src)
           end
	 | _ => X86Spill.spill(instr, reg, slot)
      end handle X86Cells.Cells =>
	(print ("in spill handler "^ Int.toString reg ^ "\n");
	 {code=[instr], proh=[reg], instr=NONE})

      fun reload{instr, reg, regmap, id} = let
	val slot = I.Displace{base=stackptr, disp=getRegLoc reg}
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
      (X86StackSpills.init(); GR32.reset(); FR.reset(); GR8.reset())

    fun ra(cluster as F.CLUSTER{regmap, ...}) = let
      fun rmPseudoPhysical(rmap, 32) = ()	(* Cells parameter *)
	| rmPseudoPhysical(rmap, n) = 
	   (Intmap.rmv rmap n; rmPseudoPhysical(rmap, n+1))

      fun cloneRegmap regmap = let
	val new = Intmap.new(32, X86Cells.Cells)
      in app (Intmap.add new) (Intmap.intMapToList regmap); new
      end

      fun setRegMap rmap (F.CLUSTER{blocks, entry, exit, blkCounter, ...}) =
	F.CLUSTER{blocks=blocks, 
		  entry=entry, 
		  exit=exit, 
		  blkCounter=blkCounter,
		  regmap=rmap}

      fun intra32 cluster = let
	val ra = IntRA32.ra IntRA32.REGISTER_ALLOCATION [] 
	val cluster' as F.CLUSTER{regmap, ...} = ra cluster 
      in spillSlotTbl := SOME regmap; cluster'
      end

      fun insertPseudoPhysical(F.CLUSTER{regmap, ...}) = let
	fun add(32) = ()
	  | add(n) = (Intmap.add regmap (n, n); add(n+1))
      in add(8)
      end

      fun preference r = let
	val pref = Intmap.map (Option.valOf (!spillSlotTbl)) r
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

      val (n,m) = X86RewritePseudo.rewrite 32 cluster

      val cluster = setRegMap regmap cluster
      val _ = 	rmPseudoPhysical(regmap, 8)
      fun fromto(n,m) = if n>m then [] else n::fromto(n+1,m)
      val cluster = intra8 (fromto(n,m)) (* preference *) cluster 
      val _ = printGraph "\t---After register allocation K=8---\n" cluster

      val _ = insertPseudoPhysical cluster
      val cluster = floatRa cluster
      val _ = printGraph "\t---After floating register allocation---\n" cluster
    in 
      (*
       spillInit(); 
       (floatRa o intra8 o (setRegMap clonedRmap) o intra32) cluster
       *)
      cluster
    end
  end (* RegAllocation *)

  val optimizerHook : (F.cluster -> F.cluster) option ref = ref NONE

  (* primitives for generation of X86 instruction flowgraphs *)
  structure FlowGraphGen = 
     FlowGraphGen(structure Flowgraph = X86FlowGraph
		  structure InsnProps = P
		  structure MLTree = X86MLTree
		  val optimize = optimizerHook
		  val output = BackPatch.bbsched o RegAllocation.ra)

  (* compilation of CPS to MLRISC *)
  structure MLTreeGen =
     MLRiscGen(structure MachineSpec=MachSpec
	       structure MLTreeComp=
		  X86(structure Flowgen=FlowGraphGen
		      structure X86Instr=I
		      structure X86MLTree=X86MLTree
		      val tempMem=I.Displace{base=stackptr, disp=I.Immed 304})
	       structure Cells=X86Cells
	       structure C=X86CpsRegs
	       structure PseudoOp=X86PseudoOps
	       structure CpsTreeify=CpsTreeify)

  fun copyProp _ = error "copyProp: not defined"
  val codegen = MLTreeGen.codegen
  val finish = BackPatch.finish
end

