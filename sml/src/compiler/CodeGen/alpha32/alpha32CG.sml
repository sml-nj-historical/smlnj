(* alpha32CG.sml --- 32 bit DEC alpha code generator
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
functor Alpha32CG(structure Emitter : EMITTER_NEW
		    where I = Alpha32Instr
		    where P = Alpha32PseudoOps) : MACHINE_GEN = 
struct

  structure I = Alpha32Instr
  structure C = Alpha32Cells
  structure R = Alpha32CpsRegs
  structure B = Alpha32MLTree.BNames
  structure F = Alpha32FlowGraph
  structure Asm	     = Alpha32AsmEmitter
  structure MLTree   = Alpha32MLTree
  structure MachSpec = Alpha32Spec
  structure CG = Control.CG

  fun error msg = ErrorMsg.impossible ("Alpha32CG." ^ msg)

  val stack = Alpha32Instr.Region.stack

  structure Alpha32Rewrite = Alpha32Rewrite(Alpha32Instr)

  (* properties of instruction set *)
  structure P = 
    Alpha32Props(structure Alpha32Instr= I
		 structure Shuffle=Alpha32Shuffle)


  (* Label backpatching and basic block scheduling *)
  structure BBSched =
    BBSched2(structure Flowgraph = F
	     structure Jumps = 
	       Alpha32Jumps(structure Instr=Alpha32Instr
			    structure Shuffle=Alpha32Shuffle)
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
    val initialSpillOffset = 128
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

    fun mvInstr(rd, rs) = I.OPERATE{oper=I.BIS, ra=rs, rb=I.REGop 31, rc=rd} 
    fun fmvInstr(fd, fs) = I.FOPERATE{oper=I.CPYS, fa=fs, fb=fs, fc=fd} 


    fun spill (stClass, stOp, getLoc, newReg, rewrite) {regmap,instr,reg,id:B.name} = let
      val offset = I.IMMop (getLoc(reg))
      fun spillInstr(src) = 
	[stClass{stOp=stOp, r=src, b=C.stackptrR, d=offset, mem=stack}]
    in
      case instr
      of I.COPY{dst as [rd], src as [rs], tmp, impl} =>
	  if rd=reg then
	    {code=spillInstr(rs),  instr=NONE,   proh=[]:int list}
	  else (case tmp
	     of SOME(I.Direct r) => let
		  val loc = I.Displace{base=C.stackptrR, disp=getLoc(r)}
		  val instr=I.COPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]}
		end
              | _ => error "spill: COPY"
	    (*esac*))
       | I.FCOPY{dst as [fd], src as [fs], tmp, impl} => 	 (* reg = fd *)
	  if reg=fd then
	    {code=spillInstr(fs),   instr=NONE,   proh=[]}
	  else (case tmp
	     of SOME(I.FDirect r) => let
		  val loc = I.Displace{base=C.stackptrR, disp=getLoc(r)}
		  val instr=I.FCOPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]}
		end
              | _ => error "spill: COPY"
	    (*esac*))
       | _ => let
	    val newR = newReg()
	    val instr' = rewrite(regmap, instr, reg, newR)
	  in {code=spillInstr(newR),  instr=SOME instr',  proh=[newR]}
	  end
    end

    fun reload (ldClass, ldOp, getLoc, newReg, rewrite) {regmap,instr,reg,id:B.name} = let
      val offset = I.IMMop (getLoc(reg))
      fun reloadInstr(dst, rest) =
	ldClass{ldOp=ldOp, r=dst, b=C.stackptrR, d=offset, mem=stack}::rest
    in 
      case instr
      of I.COPY{dst=[rd], src=[rs], ...} =>	(* reg = rs *)
	   {code=reloadInstr(rd, []),   proh=[]:int list}
       | I.FCOPY{dst=[fd], src=[fs], ...} =>	(* reg = fs *)
	   {code=reloadInstr(fd, []), proh=[]}
       | _ => let
	     val newR = newReg()
	     val instr' = rewrite(regmap, instr, reg, newR)
	   in {code=reloadInstr(newR, [instr']), proh=[newR]}
	   end
    end

    fun spillInit () = 
      (spillOffset := initialSpillOffset;
       regSpills := Intmap.new(8, RegSpills);
       fregSpills := Intmap.new(8, FregSpills))

    structure GR = GetReg(val nRegs=32 val available=R.availR)
    structure FR = GetReg(val nRegs=32 val available=R.availF)

    structure Alpha32Ra = 
       Alpha32RegAlloc(structure P = P
		       structure I = Alpha32Instr
		       structure F = F
		       structure Asm = Asm)

    (* register allocation for general purpose registers *)
    structure IntRa = 
      Alpha32Ra.IntRa
        (structure RaUser = struct
           structure I = Alpha32Instr
	   structure B = B

	   val getreg = GR.getreg
	   val spill = spill(I.STORE,I.STL, getRegLoc, C.newReg, 
			     Alpha32Rewrite.rewriteDef)
	   val reload = reload(I.LOAD, I.LDL, getRegLoc, C.newReg, 
			       Alpha32Rewrite.rewriteUse)
	   val nFreeRegs = length R.availR
	   val dedicated = R.dedicatedR
	   fun copyInstr((rds, rss), I.COPY{tmp, ...}) = 
 	     I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
         end)

    (* register allocation for floating point registers *)
    structure FloatRa = 
      Alpha32Ra.FloatRa
        (structure RaUser = struct
	   structure I = Alpha32Instr
	   structure B = B

	   val getreg = FR.getreg
	   val spill = spill (I.FSTORE, I.STT, getFregLoc, C.newFreg,
			      Alpha32Rewrite.frewriteDef)
	   val reload = reload (I.FLOAD, I.LDT, getFregLoc, C.newFreg,
				Alpha32Rewrite.frewriteUse)
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
     MLRiscGen(structure MachineSpec=Alpha32Spec
	       structure MLTreeComp=
		  Alpha32(structure Flowgen=FlowGraphGen
			  structure Alpha32Instr=Alpha32Instr
			  structure Alpha32MLTree=Alpha32MLTree
			  structure PseudoInstrs=Alpha32PseudoInstrs)
	       structure Cells=Alpha32Cells
	       structure C=Alpha32CpsRegs
	       structure ConstType=Alpha32Const
	       structure PseudoOp=Alpha32PseudoOps)

  val copyProp = RegAllocation.cp
  val codegen = MLTreeGen.codegen
  val finish = BBSched.finish
end


(*
 * $Log: alpha32CG.sml,v $
 * Revision 1.5  1998/10/06 13:59:56  george
 * Flowgraph has been removed from modules that do not need it -- [leunga]
 *
 * Revision 1.4  1998/07/25 03:05:32  george
 *   changes to support block names in MLRISC
 *
 * Revision 1.3  1998/05/23 14:09:10  george
 *   Fixed RCS keyword syntax
 *
 *)
