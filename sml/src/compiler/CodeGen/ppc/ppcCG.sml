functor PPCCG
  (structure Emitter : EMITTER_NEW
     where I = PPCInstr and P = PPCPseudoOps) : MACHINE_GEN = 
struct
  structure I = PPCInstr
  structure C = PPCCells
  structure R = PPCCpsRegs
  structure B = PPCMLTree.BNames
  structure F = PPCFlowGraph
  structure Asm	     = PPCAsmEmitter
  structure MLTree   = PPCMLTree
  structure MachSpec = PPCSpec
  structure CG = Control.CG

  fun error msg = ErrorMsg.impossible ("PPCCG." ^ msg)

  val stack = PPCInstr.Region.stack
  structure PPCRewrite = PPCRewrite(PPCInstr)

  (* properties of instruction set *)
  structure P = 
    PPCProps(structure PPCInstr= I
		 structure Shuffle=PPCShuffle)

  (* Label backpatching and basic block scheduling *)
  structure BBSched =
    BBSched2(structure Flowgraph = F
	     structure Jumps = 
	       PPCJumps(structure Instr=PPCInstr
			    structure Shuffle=PPCShuffle)
	     structure Emitter = Emitter)

  (* flow graph pretty printing routine *)
  structure PrintFlowGraph = 
     PrintFlowGraphFn (structure FlowGraph = F
                       structure Emitter   = Asm)

  (* register allocation *)
  structure RegAllocation : 
    sig
      val ra : F.cluster -> F.cluster
      val cp : F.cluster -> F.cluster
    end =
  struct

   (* spill area management *)
    val initialSpillOffset = 144
    val spillOffset = ref initialSpillOffset
    fun newOffset n =
	if n > 4096
	then error "newOffset - spill area is too small"
	else spillOffset := n
    exception RegSpills and FregSpills

    val regSpills : int Intmap.intmap ref = ref(Intmap.new(0, RegSpills))
    val fregSpills : int Intmap.intmap ref = ref(Intmap.new(0, FregSpills))

    (* get spill location for general registers *)
    fun getRegLoc reg = Intmap.map (!regSpills) reg
      handle RegSpills => let
	  val offset = !spillOffset
	in
	  newOffset(offset+4);
	  Intmap.add (!regSpills) (reg, offset);
	  offset
        end

    (* get spill location for floating registers *)
    fun getFregLoc freg = Intmap.map (!fregSpills) freg
      handle FregSpills => let
	  val offset = !spillOffset
	  val fromInt = Word.fromInt
	  val aligned = Word.toIntX(Word.andb(fromInt offset+0w7, fromInt ~8))
	in
	  newOffset(aligned+8);
	  Intmap.add (!fregSpills) (freg, aligned);
	  aligned
	end

    fun spill {regmap,instr,reg,id:B.name} = let
      val offset = I.ImmedOp (getRegLoc(reg))
      fun spillInstr(src) = 
	[I.ST{sz=I.Word, rs=src, ra=C.stackptrR, d=offset, mem=stack}]
    in
      case instr
       of I.COPY{dst as [rd], src as [rs], tmp, impl} =>
	  if rd=reg then
	    {code=spillInstr(rs),  instr=NONE,   proh=[]:int list}
	  else (case tmp
	     of SOME(I.Direct r) => let
		  val disp = I.ImmedOp(getRegLoc(r))
		  val loc = I.Displace{base=C.stackptrR, disp=disp}
		  val instr=I.COPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]}
		end
              | _ => error "spill: COPY"
	    (*esac*))
       | _ => let
	    val newR = C.newReg()
	    val instr' = PPCRewrite.rewriteDef(regmap, instr, reg, newR)
	  in {code=spillInstr(newR),  instr=SOME instr',  proh=[newR]}
	  end
    end

    fun fspill {regmap,instr,reg,id:B.name} = let
      val offset = I.ImmedOp (getFregLoc(reg))
      fun spillInstr(src) = 
	[I.ST{sz=I.Double, rs=src, ra=C.stackptrR, d=offset, mem=stack}]
    in
      case instr
      of I.FCOPY{dst as [fd], src as [fs], tmp, impl} => 	 (* reg = fd *)
       	  if reg=fd then
	    {code=spillInstr(fs),   instr=NONE,   proh=[]}
	  else (case tmp
	     of SOME(I.FDirect r) => let
		  val disp=I.ImmedOp(getFregLoc(r))
		  val loc = I.Displace{base=C.stackptrR, disp=disp}
		  val instr=I.FCOPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]}
		end
              | _ => error "spill: COPY"
	    (*esac*))
       | _ => let
	    val newR = C.newFreg()
	    val instr' = PPCRewrite.frewriteDef(regmap, instr, reg, newR)
	  in {code=spillInstr(newR),  instr=SOME instr',  proh=[newR]}
	  end
    end

    fun reload{regmap,instr,reg,id:B.name} = let
      val offset = I.ImmedOp (getRegLoc(reg))
      fun reloadInstr(dst, rest) =
	I.L{sz=I.Word, rt=dst, ra=C.stackptrR, d=offset, mem=stack}::rest
    in 
      case instr
      of I.COPY{dst=[rd], src=[rs], ...} =>	(* reg = rs *)
	   {code=reloadInstr(rd, []),   proh=[]:int list}
       | _ => let
	     val newR = C.newReg()
	     val instr' = PPCRewrite.rewriteUse(regmap, instr, reg, newR)
	   in {code=reloadInstr(newR, [instr']), proh=[newR]}
	   end
    end

    fun freload {regmap, instr, reg, id:B.name} = let
      val offset = I.ImmedOp (getFregLoc(reg))
      fun reloadInstr(dst, rest) =
	I.L{sz=I.Double, rt=dst, ra=C.stackptrR, d=offset, mem=stack}::rest
    in 
      case instr
      of I.FCOPY{dst=[fd], src=[fs], ...} =>	(* reg = fs *)
	   {code=reloadInstr(fd, []), proh=[]}
       | _ => let
	     val newR = C.newFreg()
	     val instr' = PPCRewrite.frewriteUse(regmap, instr, reg, newR)
	   in {code=reloadInstr(newR, [instr']), proh=[newR]}
	   end
    end

    fun spillInit () = 
      (spillOffset := initialSpillOffset;
       regSpills := Intmap.new(8, RegSpills);
       fregSpills := Intmap.new(8, FregSpills))

    structure GR = GetReg(val nRegs=32 val available=R.availR)
    structure FR = GetReg(val nRegs=32 val available=R.availF)

    structure PPCRa = 
       PPCRegAlloc(structure P = P
		       structure I = PPCInstr
		       structure F = F
		       structure Asm = Asm)

    (* register allocation for general purpose registers *)
    structure IntRa = 
      PPCRa.IntRa
        (structure RaUser = struct
           structure I = PPCInstr
	   structure B = B

	   val getreg = GR.getreg
	   val spill = spill
	   val reload = reload
	   val nFreeRegs = length R.availR
	   val dedicated = R.dedicatedR
	   fun copyInstr((rds, rss), I.COPY{tmp, ...}) = 
 	     I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
         end)

    (* register allocation for floating point registers *)
    structure FloatRa = 
      PPCRa.FloatRa
        (structure RaUser = struct
	   structure I = PPCInstr
	   structure B = B

	   val getreg = FR.getreg
	   val spill = fspill
	   val reload = freload 
	   val nFreeRegs = length R.availF
	   val dedicated = R.dedicatedF
	   fun copyInstr((fds, fss), I.FCOPY{tmp, ...}) = 
	     I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}
         end)

    val iRegAlloc = IntRa.ra IntRa.REGISTER_ALLOCATION
    val fRegAlloc = FloatRa.ra FloatRa.REGISTER_ALLOCATION
    val iCopyProp = IntRa.ra IntRa.COPY_PROPAGATION
    val fCopyProp = FloatRa.ra FloatRa.COPY_PROPAGATION

    fun ra cluster = let
      val pg = PrintFlowGraph.printCluster TextIO.stdOut
      fun intRa cluster = (GR.reset(); iRegAlloc cluster)
      fun floatRa cluster = (FR.reset(); fRegAlloc cluster)
    in spillInit(); (floatRa o intRa) cluster
    end
    val cp = fCopyProp o iCopyProp
  end (* RegAllocation *)
  
  val optimizerHook : (F.cluster->F.cluster) option ref = ref NONE

 (* primitives for generation of DEC alpha instruction flowgraphs *)
  structure FlowGraphGen =
     FlowGraphGen(structure Flowgraph = F
		  structure InsnProps = P
		  structure MLTree = MLTree
		  val optimize = optimizerHook
		  val output = BBSched.bbsched o RegAllocation.ra)

  (* compilation of CPS to MLRISC *)
  structure MLTreeGen = 
     MLRiscGen(structure MachineSpec=PPCSpec
	       structure MLTreeComp=
		  PPC(structure Flowgen=FlowGraphGen
		      structure PPCInstr=PPCInstr
		      structure PPCMLTree=PPCMLTree
		      structure PseudoInstrs=
			PPCPseudoInstr(structure Instr=PPCInstr)
		      val rs6000flag=false)
	       structure Cells=PPCCells
	       structure C=PPCCpsRegs
	       structure ConstType=PPCConst
	       structure PseudoOp=PPCPseudoOps)

  val copyProp = RegAllocation.cp
  val codegen = MLTreeGen.codegen
  val finish = BBSched.finish
end

