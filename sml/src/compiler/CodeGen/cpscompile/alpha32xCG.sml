(* alpha32CG.sml --- 32 bit DEC alpha code generator
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)


functor Alpha32XCG(structure Emitter : EMITTER_NEW
		    where I = Alpha32Instr
		    where F = Alpha32FlowGraph) :
  sig
    structure MLTreeGen : CPSGEN 
    val finish : unit -> unit
  end = 
struct

  structure I = Alpha32Instr
  structure C = Alpha32Cells
  structure R = Alpha32CpsRegs
  structure MLTree = Alpha32MLTree
  structure Region = Alpha32Instr.Region


  fun error msg = ErrorMsg.impossible ("Alpha32CG." ^ msg)

  val stack = Alpha32Instr.Region.stack

  structure Alpha32Rewrite = Alpha32Rewrite(Alpha32Instr)

  (* properties of instruction set *)
  structure Alpha32Props = 
    Alpha32Props(structure Alpha32Instr= I val exnptrR = [14])


  (* Label backpatching and basic block scheduling *)
  structure BBSched =
    BBSched2(structure Flowgraph = Alpha32FlowGraph
	     structure Jumps = 
	       Alpha32Jumps(structure Instr=Alpha32Instr
			    structure Shuffle=Alpha32Shuffle)
	     structure Emitter = Emitter
	     structure Scheduler = NoScheduler(Alpha32Instr))

  fun error msg = ErrorMsg.impossible ("Alpha32CG." ^ msg)

  val stack = Region.stack

  (* register allocation *)
  structure RegAllocation : 
    sig
      val ra : Alpha32FlowGraph.cluster -> Alpha32FlowGraph.cluster
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

    fun spill (stClass, stOp, getLoc, newReg, rewrite) {regmap, instr, reg} = let
      val offset = getLoc(reg)
      fun spillInstr(src) = 
	[stClass{stOp=stOp, r=src, b=C.stackptrR, d=I.IMMop offset, mem=stack}]
    in
      case instr
      of I.COPY{dst as [rd], src as [rs], tmp, impl} =>
	  if rd=reg then
	    {code=spillInstr(rs),  instr=NONE,   proh=[]:int list}
	  else (case tmp
	     of SOME(I.Direct r) => let
		  val loc = I.Displace{base=C.stackptrR, disp=offset}
		  val instr=I.COPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]:int list}
		end
              | _ => error "spill: COPY"
	    (*esac*))
       | I.FCOPY{dst as [fd], src as [fs], tmp, impl} => 
	  if reg=fd then
	    {code=spillInstr(fs),   instr=NONE,   proh=[]}
	  else (case tmp
	     of SOME(I.FDirect r) => let
		  val loc = I.Displace{base=C.stackptrR, disp=offset}
		  val instr=I.FCOPY{dst=dst, src=src, tmp=SOME(loc), impl=impl}
		in {code=[], instr=SOME instr, proh=[]:int list}
		end
              | _ => error "spill: COPY"
	    (*esac*))
       | _ => let
	    val newR = newReg()
	    val instr' = rewrite(regmap, instr, reg, newR)
	  in {code=spillInstr(newR),  instr=SOME instr',  proh=[newR]}
	  end
    end


    fun reload (ldClass, ldOp, getLoc, newReg, rewrite) {regmap, instr, reg} = let
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
       Alpha32RegAlloc(structure P = Alpha32Props
		       structure I = Alpha32Instr
		       structure F = Alpha32FlowGraph
		       structure Asm = Alpha32AsmEmitter)

    (* register allocation for general purpose registers *)
    structure IntRa = 
      Alpha32Ra.IntRa
        (structure RaUser = struct
           structure I = Alpha32Instr

	   val getreg = GR.getreg
	   val spill = spill(I.STORE, I.STL, getRegLoc, C.newReg, 
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

    fun ra cluster = let
      fun intRa cluster = (GR.reset(); iRegAlloc cluster)
      fun floatRa cluster = (FR.reset(); fRegAlloc cluster)
    in spillInit(); (floatRa o intRa) cluster
    end
  end (* RegAllocation *)

  val codegen = BBSched.bbsched o RegAllocation.ra

  (* primitives for generation of DEC alpha instruction flowgraphs *)
  structure FlowGraphGen = 
     FlowGraphGen(structure Flowgraph = Alpha32FlowGraph
		  structure InsnProps = Alpha32Props
		  structure MLTree = MLTree
		  val codegen = codegen)

  (* compilation of CPS to MLRISC *)
  structure MLTreeGen = 
     MLRiscGen(structure MachineSpec=Alpha32XSpec
	       structure MLTreeComp=
		  Alpha32(structure Flowgen=FlowGraphGen
			  structure Alpha32Instr=Alpha32Instr
			  structure Alpha32MLTree=Alpha32MLTree
			  structure PseudoInstrs=Alpha32PseudoInstrs)
	       structure Cells=Alpha32Cells
	       structure C=Alpha32CpsRegs
	       structure ConstType=Alpha32Const
	       structure PseudoOp=Alpha32PseudoOps)

  val finish = BBSched.finish
end


(*
 * $Log: alpha32xCG.sml,v $
 * Revision 1.12  1998/02/17 02:55:46  george
 *     The spill and reload functions take a register map, incase
 *   the instruction needs to be rewritten to use fresh temps.
 *
 * Revision 1.11  1998/02/16 13:58:28  george
 *   A register allocated temp is now associated with parallel COPYs
 *   instead of a dedicated register. The temp is used to break cycles.
 *
 * Revision 1.10  1998/02/13 17:20:58  george
 *   Functorized pseudoOps over the machine spec to get access to the
 *   Tag structure.
 *
 * Revision 1.9  1997/09/17 17:15:20  george
 *   dedicated registers are now part of the CPSREGS interface
 *
 * Revision 1.8  1997/08/29  11:04:03  george
 *   Spill area now starts at a new offset to account for the
 *   divl, divlu addresses on the stack.
 *
 * Revision 1.7  1997/07/28  20:04:42  george
 *   Added support for regions
 *
 * Revision 1.6  1997/07/17  12:35:53  george
 *   The constant type used to specialize MLTrees is now done more compactly.
 *
 * Revision 1.5  1997/07/15  16:01:53  dbm
 *   Change in where structure syntax.
 *
 * Revision 1.4  1997/07/02  13:25:28  george
 *   Generated better spill code, in which a new temporary is introduced
 *   to represent the register being spilled.
 *
 * Revision 1.3  1997/05/20  12:17:24  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.2  1997/03/06  19:08:49  george
 *   Use a more sensible union-find data structure to do the job.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:33  george
 *   Version 109.24
 *
 *)
