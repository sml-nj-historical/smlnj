functor HppaCG(structure Emitter : EMITTER_NEW
		 where P = HppaPseudoOps
	         and I = HppaInstr) :
		(*  and  structure I = HppaInstr -- redundant *)
  sig
    structure MLTreeGen : CPSGEN 
    val finish : unit -> unit
  end = 
struct
  structure I = HppaInstr
  structure C = HppaCells
  structure R = HppaCpsRegs
  structure CG = Control.CG
  structure Region = I.Region
  structure B = HppaMLTree.BNames

  fun error msg = ErrorMsg.impossible ("HppaCG." ^ msg)

  structure HppaRewrite = HppaRewrite(HppaInstr)

  (* properties of instruction set *)
  structure HppaProps = 
    HppaProps(structure HppaInstr = I 
	      structure Shuffle = HppaShuffle)

  
  (* Label backpatching and basic block scheduling *)
  structure BBSched =
    BBSched2(structure Flowgraph = HppaFlowGraph
	     structure Jumps = 
	       HppaJumps(structure Instr=HppaInstr
			 structure Shuffle=HppaShuffle)
	     structure Emitter = Emitter)

  (* flow graph pretty printing routine *)
  structure PrintFlowGraph = 
     PrintFlowGraphFn (structure FlowGraph = HppaFlowGraph
                       structure Emitter   = HppaAsmEmitter)
  
  (* register allocation *)
  structure RegAllocation : 
    sig
      val ra : HppaFlowGraph.cluster -> HppaFlowGraph.cluster
      val cp : HppaFlowGraph.cluster -> HppaFlowGraph.cluster
    end =
  struct
   (* spill area management *)
    val itow      = Word.fromInt
    val wtoi      = Word.toIntX
    val stack     = Region.stack

    fun fromto(n, m) = if n>m then [] else n :: fromto(n+1, m)

    fun low11(n) = wtoi(Word.andb(itow n, 0wx7ff))
    fun high21(n) = wtoi(Word.~>>(itow n, 0w11))

    val initialSpillOffset = 116	(* from runtime system *)
    val spillOffset = ref initialSpillOffset
    fun newOffset n = 
      if n > 4096 then error "incOffset - spill area too small"
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

    fun mvInstr(rd,rs) = I.ARITH{a=I.OR, r1=rs, r2=0, t=rd}
    fun fmvInstr(fd,fs) = I.FUNARY{fu=I.FCPY, f=fs, t=fd}

    fun spillInit () = 
      (spillOffset := initialSpillOffset;
       regSpills := Intmap.new(8, RegSpills);
       fregSpills := Intmap.new(8, FregSpills))

    (* spill general register *)
    fun spillR {regmap, instr, reg, id} = let
      val loc = getRegLoc reg
      fun spillInstr(r) = 
         [I.STORE{st=I.STW, b=C.stackptrR, d=I.IMMED(~loc), r=r, mem=stack}]
    in
      Control.MLRISC.int_spills := !Control.MLRISC.int_spills + 1;
      case instr
      of I.COPY{dst as [rd], src as [rs], tmp, impl} => 
	  if reg=rd then
	    {code=spillInstr(rs), instr=NONE, proh=[]}
	  else (case tmp
	     of SOME(I.Direct r) => let
		  val loc=I.Displace{base=C.stackptrR, disp= ~loc}
		  val instr=I.COPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]}
		end
	      | _ => error "spill: MOVE"
	    (*esac*))
       | _ => let
	   val newR = C.newReg()
	   val instr' = HppaRewrite.rewriteDef(regmap, instr, reg, newR)
	 in {code=spillInstr newR, instr=SOME instr', proh=[newR]}
	 end
    end

    (* reload general register *)
    fun reloadR {regmap, instr, reg, id} = let
      val loc = getRegLoc(reg)
      fun reloadInstr(r) = 
          I.LOADI{li=I.LDW, i=I.IMMED(~loc), r=C.stackptrR, t=r, mem=stack}
    in
      Control.MLRISC.int_reloads := !Control.MLRISC.int_reloads + 1;
      case instr 
      of I.COPY{dst=[rd], src=[rs], ...} => {code=[reloadInstr(rd)],  proh=[]}
       | _ => let
	   val newR = C.newReg()
	   val instr' = HppaRewrite.rewriteUse(regmap, instr, reg, newR)
	 in {code=[reloadInstr(newR), instr'], proh=[newR]}
	 end
    end

    fun spillF {regmap, instr, reg, id} = let
      val disp = getFregLoc reg
      val tmpR = C.asmTmpR
      fun spillInstrs(reg) = 
        [I.LDIL{i=I.IMMED(high21(~disp)), t=tmpR},
	 I.LDO{i=I.IMMED(low11(~disp)), b=tmpR, t=tmpR},
	 I.FSTOREX{fstx=I.FSTDX, b=C.stackptrR, x=tmpR, r=reg, mem=stack}]
    in
      Control.MLRISC.float_spills := !Control.MLRISC.float_spills + 1;
      case instr
      of I.FCOPY{dst as [fd], src as [fs], tmp, impl} => 
	  if fd=reg then
	    {code=spillInstrs(fs), instr=NONE, proh=[]}
	  else (case tmp
	     of SOME(I.FDirect f) => let
		  val loc=I.Displace{base=C.stackptrR, disp= ~disp}
		  val instr=I.FCOPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]}
		end
	      | _ => error "spillF: FCOPY"
	    (*esac*))
       | _ => let
	   val newF = C.newFreg()
	   val instr' = HppaRewrite.frewriteDef(regmap, instr, reg, newF)
	 in {code=spillInstrs(newF), instr=SOME instr', proh=[newF]}
	 end
    end

    fun reloadF {regmap, instr, reg, id:B.name} = let
      val disp = getFregLoc reg
      val tmpR = C.asmTmpR
      fun reloadInstrs(reg, rest) = 
	 I.LDIL{i=I.IMMED(high21(~disp)), t=tmpR} ::
	 I.LDO{i=I.IMMED(low11(~disp)), b=tmpR, t=tmpR} ::
	 I.FLOADX{flx=I.FLDDX, b=C.stackptrR, x=tmpR, t=reg, mem=stack} :: rest
    in
      Control.MLRISC.float_reloads := !Control.MLRISC.float_reloads + 1;
      case instr
      of I.FCOPY{dst=[fd], src=[fs], ...} => {code=reloadInstrs(fd, []), proh=[]}
       | _ => let
	   val newF = C.newFreg()
	   val instr' = HppaRewrite.frewriteUse(regmap, instr, reg, newF)
	 in {code=reloadInstrs(newF, [instr']), proh=[newF]}
	 end
    end

    structure GR = GetReg(val nRegs = 32 val available = R.availR)
    structure FR = GetReg(val nRegs = 32 val available = R.availF)

    structure HppaRa = 
      HppaRegAlloc(structure P = HppaProps
		   structure I = HppaInstr
		   structure F = HppaFlowGraph
		   structure Asm = HppaAsmEmitter)

    (* register allocation for general purpose registers *)
    structure IntRa = 
      HppaRa.IntRa
        (structure RaUser = struct
	    structure I = HppaInstr
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
      HppaRa.FloatRa
        (structure RaUser = struct
	    structure I = HppaInstr
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

  (* primitives for generation of HPPA instruction flowgraphs *)
  structure FlowGraphGen = 
     FlowGraphGen(structure Flowgraph = HppaFlowGraph
		  structure InsnProps = HppaProps
		  structure MLTree = HppaMLTree
		  val codegen = codegen)

  structure HppaMillicode = 
    HppaMillicode(structure MLTree=HppaMLTree
		  structure Instr=HppaInstr)

  structure HppaLabelComp = 
    HppaLabelComp(structure MLTree=HppaMLTree
		  structure Instr=HppaInstr)

  (* compilation of CPS to MLRISC *)
  structure MLTreeGen = 
     MLRiscGen(structure MachineSpec=HppaSpec
	       structure MLTreeComp=
		 Hppa(structure Flowgen=FlowGraphGen
		      structure HppaInstr = HppaInstr
		      structure HppaMLTree = HppaMLTree
		      structure MilliCode=HppaMillicode
		      structure LabelComp=HppaLabelComp)
	       structure Cells=HppaCells
	       structure C=HppaCpsRegs
	       structure ConstType=HppaConst
	       structure PseudoOp=HppaPseudoOps)

  val finish = BBSched.finish
end

(*
 * $Log: hppaCG.sml,v $
 * Revision 1.6  1998/10/19 13:50:42  george
 * *** empty log message ***
 *
 * Revision 1.5  1998/10/06 13:59:59  george
 * Flowgraph has been removed from modules that do not need it -- [leunga]
 *
 * Revision 1.4  1998/07/25 03:05:34  george
 *   changes to support block names in MLRISC
 *
 * Revision 1.3  1998/05/23 14:09:20  george
 *   Fixed RCS keyword syntax
 *
 *)
