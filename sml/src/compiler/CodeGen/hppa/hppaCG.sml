functor HppaCG(structure Emitter : EMITTER_NEW
		 where F = HppaFlowGraph) :
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

  fun error msg = ErrorMsg.impossible ("HppaCG." ^ msg)

  structure HppaRewrite = HppaRewrite(HppaInstr)

  (* properties of instruction set *)
  structure HppaProps = HppaProps(structure HppaInstr = I val exnptrR = [6])

  
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
    fun spillR {regmap, instr, reg} = let
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
    fun reloadR {regmap, instr, reg} = let
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

    fun spillF {regmap, instr, reg} = let
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

    fun reloadF {regmap, instr, reg} = let
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
 * Revision 1.14  1998/02/17 02:57:55  george
 *   The spill and reload functions take a register map, incase
 *   the instruction needs to be rewritten to use fresh temps.
 *
 * Revision 1.13  1998/02/16 13:58:29  george
 *   A register allocated temp is now associated with parallel COPYs
 *   instead of a dedicated register. The temp is used to break cycles.
 *
 * Revision 1.12  1998/02/13 17:21:07  george
 *   Functorized pseudoOps over the machine spec to get access to the
 *   Tag structure.
 *
 * Revision 1.11  1997/09/29 20:58:45  george
 *   Propagate region information through instruction set
 *
# Revision 1.10  1997/09/17  17:15:34  george
#   dedicated registers are now part of the CPSREGS interface
#
# Revision 1.9  1997/08/29  11:05:27  george
#   Spill area now starts at a new offset to account for the
#   new mulu address on the stack.
#
# Revision 1.8  1997/07/28  20:05:06  george
#   Added support for regions
#
# Revision 1.7  1997/07/17  12:37:33  george
#   The constant type used to specialize MLTrees is now done more compactly.
#
# Revision 1.6  1997/07/15  16:08:06  dbm
#   Change in where structure syntax.
#
# Revision 1.5  1997/07/03  13:56:49  george
#   Added support for FCOPY.
#
# Revision 1.4  1997/07/02  13:25:38  george
#   Generated better spill code, in which a new temporary is introduced
#   to represent the register being spilled.
#
# Revision 1.3  1997/06/13  15:29:43  george
#   Modified codegen to print flowgraph at the end of each phase -- leunga
#
# Revision 1.2  1997/05/20  12:21:50  dbm
#   SML '97 sharing, where structure.
#
# Revision 1.1  1997/04/19  18:17:46  george
#   Version 109.27
#
 * Revision 1.2  1997/04/16  02:25:57  george
 *   Instruction selection is now parameterized over modules that
 *   describes how to generate millicalls, and translate trees
 *   involving labels.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:34  george
 *   Version 109.24
 *
 *)
