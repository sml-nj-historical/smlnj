functor HppaCG(structure Emitter : INSTRUCTION_EMITTER
		 where P = HppaPseudoOps
	         and I = HppaInstr 
                 and S.B = HppaMLTree.BNames) : MACHINE_GEN = 
struct
  structure I = HppaInstr
  structure C = HppaCells
  structure R = HppaCpsRegs
  structure Ctrl = Control.MLRISC
  structure Region = I.Region
  structure B = HppaMLTree.BNames
  structure F = HppaFlowGraph
  structure MachSpec = HppaSpec
  structure Asm = HppaAsmEmitter

  fun error msg = ErrorMsg.impossible ("HppaCG." ^ msg)

  structure HppaRewrite = HppaRewrite(HppaInstr)

  (* properties of instruction set *)
  structure P = HppaProps(I)

  structure FreqProps = FreqProps(P)
  
  (* Label backpatching and basic block scheduling *)
  structure BBSched =
    SpanDependencyResolution
            (structure Flowgraph = F
	     structure Emitter = Emitter
	     structure Jumps =
	       HppaJumps(structure Instr=HppaInstr
			 structure Shuffle=HppaShuffle)
	     structure DelaySlot = HppaDelaySlots(structure I = I
                                                  structure P = P)
	     structure Props = P
            )

  val intSpillCnt = Ctrl.getCounter "ra-int-spills"
  val floatSpillCnt = Ctrl.getCounter "ra-float-spills"
  val intReloadCnt = Ctrl.getCounter "ra-int-reloads"
  val floatReloadCnt = Ctrl.getCounter "ra-float-reloads"
  
  (* register allocation *)
  structure RegAllocation : 
    sig
      val ra : F.cluster -> F.cluster
      val cp : F.cluster -> F.cluster
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
    fun fmvInstr(fd,fs) = I.FUNARY{fu=I.FCPY_D, f=fs, t=fd}

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
      intSpillCnt := !intSpillCnt + 1;
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
      intReloadCnt := !intReloadCnt + 1;      
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
      floatSpillCnt := !floatSpillCnt + 1;
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
      floatReloadCnt := !floatReloadCnt + 1;
      case instr
      of I.FCOPY{dst=[fd], src=[fs], ...} => {code=reloadInstrs(fd, []), proh=[]}
       | _ => let
	   val newF = C.newFreg()
	   val instr' = HppaRewrite.frewriteUse(regmap, instr, reg, newF)
	 in {code=reloadInstrs(newF, [instr']), proh=[newF]}
	 end
    end

    structure GR = GetReg(val first=0 val nRegs = 32 val available = R.availR)
    structure FR = GetReg(val first=32 val nRegs = 32 val available = R.availF)

    structure HppaRa = 
      HppaRegAlloc(structure P = P
		   structure I = HppaInstr
		   structure F = F
		   structure Asm = Asm)

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

    val iRegAlloc = IntRa.ra IntRa.REGISTER_ALLOCATION []
    val fRegAlloc = FloatRa.ra FloatRa.REGISTER_ALLOCATION []
    val icp       = IntRa.ra IntRa.COPY_PROPAGATION []
    val fcp       = FloatRa.ra FloatRa.COPY_PROPAGATION []
    val cp        = fcp o icp

    fun ra cluster = let
      fun intRa cluster = (GR.reset(); iRegAlloc cluster)
      fun floatRa cluster = (FR.reset(); fRegAlloc cluster)
    in
      spillInit();
      (floatRa o intRa) cluster
    end
  end 

  val optimizerHook : (F.cluster->F.cluster) option ref = ref NONE

  (* primitives for generation of HPPA instruction flowgraphs *)
  structure FlowGraphGen = 
     ClusterGen(structure Flowgraph = F
	        structure InsnProps = P
		structure MLTree = HppaMLTree
		structure Stream = Emitter.S
		val optimize = optimizerHook
		val output = BBSched.bbsched o RegAllocation.ra)

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
		 Hppa(structure HppaInstr = HppaInstr
		      structure Stream = HppaStream
		      structure HppaMLTree = HppaMLTree
		      structure MilliCode=HppaMillicode
		      structure LabelComp=HppaLabelComp
		      val costOfMultiply = ref 7
		      val costOfDivision = ref 7
                     )
	       structure Flowgen=FlowGraphGen
	       structure Cells=HppaCells
	       structure C=HppaCpsRegs
	       structure PseudoOp=HppaPseudoOps)

  val copyProp = RegAllocation.cp
  val codegen = MLTreeGen.codegen
  val finish = BBSched.finish
end

