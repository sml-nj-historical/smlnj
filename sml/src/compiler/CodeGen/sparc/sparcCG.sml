functor SparcCG(structure Emitter : EMITTER_NEW
		 where I = SparcInstr and P = SparcPseudoOps) :
  sig
    structure MLTreeGen : CPSGEN 
    val finish : unit -> unit
  end = 
struct
  structure I = SparcInstr
  structure C = SparcCells
  structure R = SparcCpsRegs
  structure CG = Control.CG
  structure B = SparcMLTree.BNames
  structure Region = I.Region

  fun error msg = ErrorMsg.impossible ("SparcCG." ^ msg)

  structure SparcRewrite = SparcRewrite(SparcInstr)

  (* properties of instruction set *)
  structure SparcProps = 
    SparcProps(structure SparcInstr = I 
	       structure Shuffle = SparcShuffle)

  
  (* Label backpatching and basic block scheduling *)
  structure SparcJumps =
     SparcJumps(structure Instr=SparcInstr
   	        structure Shuffle=SparcShuffle)
  structure BBSched =
      SpanDependencyResolution(
         structure Flowgraph = SparcFlowGraph
	 structure Jumps = SparcJumps
	 structure Emitter = Emitter
         structure DelaySlot = SparcDelaySlotProps(structure I=SparcInstr
                                                   structure P=SparcProps)
         structure Props = SparcProps
      )

  (* flow graph pretty printing routine *)
  structure PrintFlowGraph = 
     PrintFlowGraphFn (structure FlowGraph = SparcFlowGraph
                       structure Emitter   = SparcAsmEmitter)

  (* register allocation *)
  structure RegAllocation : 
    sig
      val ra : SparcFlowGraph.cluster -> SparcFlowGraph.cluster
      val cp : SparcFlowGraph.cluster -> SparcFlowGraph.cluster
    end =
  struct
   (* spill area management *)
    val itow      = Word.fromInt
    val stack     = Region.stack

    fun fromto(n, m) = if n>m then [] else n :: fromto(n+1, m)

    val initialSpillOffset = 116	(* from runtime system *)
    val spillOffset = ref initialSpillOffset
    fun newOffset n = 
      if n > 3800 then error "incOffset - spill area too small"
      else spillOffset := n
    exception RegSpills and FregSpills
    val regSpills : int Intmap.intmap ref = ref(Intmap.new(0, RegSpills))
    val fregSpills : int Intmap.intmap ref = ref(Intmap.new(0, FregSpills))

    (* get spill location for register *)
    fun getRegLoc reg = Intmap.map (!regSpills) reg
        handle RegSpills => let
	    val offset = !spillOffset
	  in
	    newOffset(offset+4);
	    Intmap.add (!regSpills) (reg, offset);
	    offset
	  end

    (* get spill location for floating register *)
    fun getFregLoc freg = Intmap.map (!fregSpills) freg
        handle FregSpills => let
  	    val spillLoc = !spillOffset
  	    val aligned = Word.toIntX (Word.andb(itow (spillLoc+7), itow ~8))
          in
	    newOffset(aligned+8);
	    Intmap.add (!fregSpills) (freg, aligned);
	    aligned
	  end

    fun mvInstr(rd,rs) = I.ARITH{a=I.OR, r=0, i=I.REG rs, d=rd, cc=false}
    fun fmvInstr(fd,fs) = I.FPop1{a=I.FMOVd, r=fs, d=fd}

    fun spillInit () = 
      (spillOffset := initialSpillOffset;
       regSpills := Intmap.new(8, RegSpills);
       fregSpills := Intmap.new(8, FregSpills))

    (* spill general register *)
    fun spillR {regmap, instr, reg, id} = let
      val loc = getRegLoc reg
      fun spillInstr(r) = 
         [I.STORE{s=I.ST, r=C.stackptrR, i=I.IMMED(loc), d=r, mem=stack}]
    in
      Control.MLRISC.int_spills := !Control.MLRISC.int_spills + 1;
      case instr
      of I.COPY{dst as [rd], src as [rs], tmp, impl} => 
	  if reg=rd then
	    {code=spillInstr(rs), instr=NONE, proh=[]}
	  else (case tmp
	     of SOME(I.Direct r) => let
		  val loc=I.Displace{base=C.stackptrR, disp=loc}
		  val instr=I.COPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]}
		end
	      | _ => error "spill: MOVE"
	    (*esac*))
       | _ => let
	   val newR = C.newReg()
	   val instr' = SparcRewrite.rewriteDef(regmap, instr, reg, newR)
	 in {code=spillInstr newR, instr=SOME instr', proh=[newR]}
	 end
    end

    (* reload general register *)
    fun reloadR {regmap, instr, reg, id} = let
      val loc = getRegLoc(reg)
      fun reloadInstr(r) = 
          I.LOAD{l=I.LD, i=I.IMMED(loc), r=C.stackptrR, d=r, mem=stack}
    in
      Control.MLRISC.int_reloads := !Control.MLRISC.int_reloads + 1;
      case instr 
      of I.COPY{dst=[rd], src=[rs], ...} => {code=[reloadInstr(rd)],  proh=[]}
       | _ => let
	   val newR = C.newReg()
	   val instr' = SparcRewrite.rewriteUse(regmap, instr, reg, newR)
	 in {code=[reloadInstr(newR), instr'], proh=[newR]}
	 end
    end

    fun spillF {regmap, instr, reg, id} = let
      val disp = getFregLoc reg
      fun spillInstrs(reg) = 
        [I.FSTORE{s=I.STDF, r=C.stackptrR, i=I.IMMED(disp), d=reg, mem=stack}]
    in
      Control.MLRISC.float_spills := !Control.MLRISC.float_spills + 1;
      case instr
      of I.FCOPY{dst as [fd], src as [fs], tmp, impl} => 
	  if fd=reg then
	    {code=spillInstrs(fs), instr=NONE, proh=[]}
	  else (case tmp
	     of SOME(I.FDirect f) => let
		  val loc=I.Displace{base=C.stackptrR, disp=disp}
		  val instr=I.FCOPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]}
		end
	      | _ => error "spillF: FCOPY"
	    (*esac*))
       | _ => let
	   val newF = C.newFreg()
	   val instr' = SparcRewrite.frewriteDef(regmap, instr, reg, newF)
	 in {code=spillInstrs(newF), instr=SOME instr', proh=[newF]}
	 end
    end

    fun reloadF {regmap, instr, reg, id} = let
      val disp = getFregLoc reg
      fun reloadInstrs(reg, rest) = 
	 I.FLOAD{l=I.LDDF, r=C.stackptrR, i=I.IMMED(disp), d=reg, mem=stack} 
            :: rest
    in
      Control.MLRISC.float_reloads := !Control.MLRISC.float_reloads + 1;
      case instr
      of I.FCOPY{dst=[fd], src=[fs], ...} => {code=reloadInstrs(fd, []), proh=[]}
       | _ => let
	   val newF = C.newFreg()
	   val instr' = SparcRewrite.frewriteUse(regmap, instr, reg, newF)
	 in {code=reloadInstrs(newF, [instr']), proh=[newF]}
	 end
    end

    structure GR = GetReg(val nRegs = 32 val available = R.availR)
    structure FR = GetReg(val nRegs = 32 val available = R.availF)

    structure SparcRa = 
      SparcRegAlloc(structure P = SparcProps
		    structure I = SparcInstr
		    structure F = SparcFlowGraph
		    structure Asm = SparcAsmEmitter)

    (* register allocation for general purpose registers *)
    structure IntRa = 
      SparcRa.IntRa
        (structure RaUser = struct
	    structure I = SparcInstr
	    structure B = B

            val getreg = GR.getreg
	    val spill = spillR
	    val reload = reloadR
	    val nFreeRegs = length R.availR
	    val dedicated = R.dedicatedR
	    fun copyInstr((rds, rss), I.COPY{tmp, ...}) = 
	      I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
         end)


    (* register allocation for floating point registers *)
    structure FloatRa = 
      SparcRa.FloatRa
        (structure RaUser = struct
	    structure I = SparcInstr
	    structure B = B

	    val getreg = FR.getreg
	    val spill = spillF
	    val reload = reloadF
	    val nFreeRegs = length R.availF
	    val dedicated = R.dedicatedF
	    fun copyInstr((fds, fss), I.FCOPY{tmp, ...}) = 
	      I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}
         end)

    val iRegAlloc = IntRa.ra IntRa.REGISTER_ALLOCATION
    val fRegAlloc = FloatRa.ra FloatRa.REGISTER_ALLOCATION
    val icp       = IntRa.ra IntRa.COPY_PROPAGATION
    val fcp       = FloatRa.ra FloatRa.COPY_PROPAGATION
    val cp        = fcp o icp

    fun ra cluster = let
      fun intRa cluster = (GR.reset(); iRegAlloc cluster)
      fun floatRa cluster = (FR.reset(); fRegAlloc cluster)
    in
      spillInit();
      (floatRa o intRa) cluster
    end
  end 

(*
  structure Opt =
      MLRISC_OptimizerF(structure F   = SparcFlowGraph
                        structure Asm = SparcAsmEmitter
                        structure P    = SparcProps
                        structure Ctrl = MLRISC_Control
                        val copy_propagation    = RegAllocation.cp
                        val register_allocation = RegAllocation.ra
                        val emit_code           = BBSched.bbsched
                       )
*)

 fun codegen cluster = let
    fun phaseToMsg(CG.AFTER_INSTR_SEL) = "After instruction selection"
      | phaseToMsg(CG.AFTER_RA) = "After register allocation"
      | phaseToMsg(CG.AFTER_SCHED) = "After instruction scheduling"
      | phaseToMsg _ = error "phaseToMsg"
    val printGraph = PrintFlowGraph.printCluster (!CG.printFlowgraphStream)
    fun doPhase (phase, f) cluster = let
      fun show(CG.PHASES(ph1, ph2)) = show ph1 orelse show ph2
        | show(ph) = (ph = phase)
      val newCluster = f cluster
    in
      if show (!CG.printFlowgraph) then
        printGraph (phaseToMsg phase) newCluster
      else ();
      newCluster
    end
    val instrSel = doPhase (CG.AFTER_INSTR_SEL, fn x => x)
    val regAlloc = doPhase (CG.AFTER_RA, RegAllocation.ra)
  in
    case !CG.printFlowgraph
    of CG.NO_PHASE => (BBSched.bbsched o RegAllocation.ra) cluster
     | phase => (BBSched.bbsched o regAlloc o instrSel) cluster
  end

  (* primitives for generation of SPARC instruction flowgraphs *)
  structure FlowGraphGen = 
     FlowGraphGen(structure Flowgraph = SparcFlowGraph
		  structure InsnProps = SparcProps
		  structure MLTree = SparcMLTree
		  val codegen = codegen)

  (* compilation of CPS to MLRISC *)
  structure MLTreeGen = 
     MLRiscGen(structure MachineSpec=SparcSpec
	       structure MLTreeComp=
		 Sparc(structure Flowgen=FlowGraphGen
		       structure SparcInstr = SparcInstr
		       structure SparcMLTree = SparcMLTree
                       structure PseudoInstrs = SparcPseudoInstrs 
                       val overflowtrap = (* tvs 0x7 *)
                           [I.Ticc{t=I.BVS,r=0,i=I.IMMED 7}]
                      )
	       structure Cells=SparcCells
	       structure C=SparcCpsRegs
	       structure ConstType=SparcConst
	       structure PseudoOp=SparcPseudoOps)

  val finish = BBSched.finish
end

(*
 * $Log: sparcCG.sml,v $
 * Revision 1.2  1998/10/06 14:00:01  george
 * Flowgraph has been removed from modules that do not need it -- [leunga]
 *
 * Revision 1.1.1.1  1998/08/05 19:37:50  george
 *   Release 110.7.4
 *
 *)
