(* mipsdepend.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor MipsInstr(structure E:ENDIAN) : MACHINSTR =
struct

structure M = MipsInstrSet
open M

val branchDelayedArch = true

datatype ikind = IK_NOP | IK_JUMP | IK_INSTR

val error = ErrorMsg.impossible

fun instrKind instr = case instr
    			of M.NOP     => IK_NOP
		         | M.JUMP _  => IK_JUMP
			 | M.BEQ _   => IK_JUMP
			 | M.BCOP1 _ => IK_JUMP
			 | _         => IK_INSTR

fun isSdiBranch (BRANCH _) = true
  | isSdiBranch (BRANCH_COP1 _) = true
  | isSdiBranch _ = false

val nop = M.NOP

fun minSize (LOADF _) = 8
  | minSize _ 	      = 4


(* sizeOf(sdi,loc) 
 *	returns the size of I under the current address assignment
 * for labels plus true if the size if the maximum possible for the sdi. 
 *)
fun sizeOf (info as INFO{addrOf,...}) =
 let val labelValue = M.labelValue info
     val hiLabelValue = M.hiLabelValue info
     val loLabelValue = M.loLabelValue info
  in fn (sdi,loc) =>
  let 
    fun lab_value (lab,k) = labelValue(POSLAB lab,k -  M.constBaseRegOffset)
    fun is_short_mem n = ~32764 <= n andalso n <= 32764
    fun is_short_branch lab = let 
        val const = addrOf lab - (loc + 4)
      in 
	  is_short_mem(const div 4)
      end
  in
      case sdi 
      of SETBASEADDR(lab,reg) => let
	   val const = M.constBaseRegOffset - (addrOf lab)
	 in 
	     if is_short_mem const then (false, 4) else (true, 12)
	 end
      | LOADADDR(_,lab,k)  =>
	  if is_short_mem (lab_value (lab,k)) then (false,4) else (true,12) 
      | LOAD(_,lab,k) =>
	  if is_short_mem (lab_value (lab,k)) then (false,4) else (true,12) 
      | LOADF(_,lab,offset,_)  => let
	  val labexp1 = (POSLAB lab,offset-M.constBaseRegOffset)
	  val labexp2 = (POSLAB lab,offset-M.constBaseRegOffset+4)
	  val labval1 = labelValue labexp1
	  val labval2 = labelValue labexp2
        in
	  if (is_short_mem labval1) andalso (is_short_mem labval2) 
	  then (false, 8)
	  else if (hiLabelValue labexp1) = (hiLabelValue labexp2)
	       then (false,16)
	       else (true,20)
        end
      | BRANCH(_,_,_,lab,_,_) =>
	  if is_short_branch lab then (false,4) else (true,20)
      | BRANCH_COP1(_,lab,_,_) =>
	  if is_short_branch lab then (false,4) else (true,20)
  end
 end

(* expand (I, n)
 *	- expands I into n bytes of machine instructions. 
 *)
fun expand info (sdi,size,loc) =
    case sdi
    of SETBASEADDR(lab,reg) =>
	  let val labexp = (NEGLAB lab,M.constBaseRegOffset)
	  in
	    case size 
	    of 4 => [M.ADD(M.baseReg,reg,LabelOp labexp)]
	     | 12 => [M.LUI(M.baseReg,HiLabOff labexp),
		      M.ADD(M.baseReg,M.baseReg,LoLabOp labexp),
		      M.ADD(M.baseReg,reg,RegOp M.baseReg)]
	     | _ => error "MipsInstrSet.expand: SETBASEADDR"
	  end
     | LOADADDR(r,lab,k) =>
	   let val labexp = (POSLAB lab,~M.constBaseRegOffset+k)
	   in case size
	      of 4 => [M.ADD(r,M.baseReg,LabelOp labexp)]
	       | 12 =>  [M.LUI(r,HiLabOff labexp),
			 M.ADD(r,r,LoLabOp labexp),
			 M.ADD(r,M.baseReg,RegOp r)]
	       | _ => error "MipsInstrSet.expand: LOADADDR"
	   end
     | LOAD(r,lab,k) =>
	   let val labexp = (POSLAB lab,~M.constBaseRegOffset+k)
	   in case size
	      of 4  => [M.LW(r,M.baseReg,LabOff labexp)]
	       | 12 => [M.LUI(r,HiLabOff labexp),
			M.ADD(r,M.baseReg,RegOp r),
			M.LW(r,r,LoLabOff labexp)]
	       | _ => error "MipsInstrSet.expand: LOAD"
	   end
     | LOADF(fp',lab,offset,tmpR) => 
	   let val Freg' fp = reg_rep fp'
	       val labexp1 = (POSLAB lab,offset-M.constBaseRegOffset)
	       val labexp2 = (POSLAB lab,offset-M.constBaseRegOffset+4)
	       val lowOff = E.low_order_offset
	   in case size 
	      of 8 => 
		     [M.LWC1(Freg(fp+lowOff),M.baseReg,LabOff labexp1),
		      M.LWC1(Freg(fp+1-lowOff),M.baseReg,LabOff labexp2)]
	       | 16 => 
		     [M.LUI(tmpR,HiLabOff labexp1),
		      M.ADD(tmpR,tmpR,RegOp M.baseReg),
		      M.LWC1(Freg(fp+lowOff),tmpR,LoLabOff labexp1),
		      M.LWC1(Freg(fp+1-lowOff),tmpR,LoLabOff labexp2)]
	       | 20 => 
		     [M.LUI(tmpR,HiLabOff labexp1),
		      M.ADD(tmpR,tmpR,LoLabOp labexp1),
		      M.ADD(tmpR,tmpR,RegOp M.baseReg),
		      M.LWC1(Freg(fp+lowOff),tmpR,Immed16Off 0),
		      M.LWC1(Freg(fp+1-lowOff),tmpR,Immed16Off 4)]
	       | _ => error "MipsInstrSet.expand: LOADF"
	   end
     | BRANCH(bool,rs,rt,tlabel,tmpR,flabel) => 
	   (case size
	    of 4  => [M.BEQ(bool,rs,rt,LabOff(POSLAB tlabel,0))]

	    | 20 => 
		  let val labexp = (POSLAB tlabel,~M.constBaseRegOffset)
		  in
		      [M.BEQ(not bool,rs,rt,LabOff(POSLAB flabel,0)),

		       M.LUI(tmpR,HiLabOff labexp),
		       M.ADD(tmpR,tmpR,LoLabOp labexp),
		       M.ADD(tmpR,tmpR,RegOp M.baseReg),
		       M.JUMP(tmpR)]
		  end
	   | _ => error "MipsInstrSet.expand: BRANCH")
     | BRANCH_COP1(bool,tlabel,tmpR,flabel) => 
	   (case size 
            of 4  => [M.BCOP1(bool,LabOff(POSLAB tlabel,0))]
             | 20 => 
		  let val labexp = (POSLAB tlabel,~M.constBaseRegOffset)
		  in
		      [M.BCOP1(not bool,LabOff(POSLAB flabel,0)),

		       M.LUI(tmpR,HiLabOff labexp),
		       M.ADD(tmpR,tmpR,LoLabOp labexp),
		       M.ADD(tmpR,tmpR,RegOp M.baseReg),
		       M.JUMP(tmpR)]
		  end
	     | _ => error "MipsInstrSet.expand: BRANCH_COP1")

(* 
 * Resources:  Mem+($1-$31)+($f0,f1-$f31)+fcc+npc+LO+HI 
 *)
val numResources 	= 68

local
    structure RId = 
	struct
	    val mem = 0
	    fun reg r = case reg_rep r 
		         of Reg' 0 => []
			  | Reg' i => [i]
			  | _ => error "MipsInstrSet.RId.reg"
	    fun freg r = case reg_rep r 
		         of Freg' i => [32+i]
			  | _ => error "MipsInstrSet.RId.freg"
	    fun fregd r = case reg_rep r 
		         of Freg' i => [32+i, 32+1+i]
			  | _ => error "MipsInstrSet.RId.fregd"
	    fun anyreg r = case reg_rep r 
		of Reg' 0 => []
		 | Reg' i => [i]
		 | Freg' i => [32+i]
	    val fcc = 64 
	    val npc = 65 
	    val LO  = 66 
	    val HI  = 67 
	    
	    val Reg'(alloc)  = reg_rep M.allocReg (* resource no. for reg = reg no. *)
	    val Reg'(exnptr) = reg_rep M.exnptrReg
	end

    fun is_allocR reg = reg_eq(reg,M.allocReg)
    fun arithOpndUse opnd = case opnd of RegOp r => RId.reg r | _ => []

    val allR = let fun f(~1,l) = l | f(i,l) = f(i-1,i::l)
	       in f(numResources-1,[])
	       end
    local
	open RId
    in
        fun arith_ud(rd,rs,ea)    = 
	  (mem::alloc::exnptr::(reg rs @ arithOpndUse ea), reg rd)
	fun logical_ud(rd,rs,ea)  = (reg rs @ arithOpndUse ea, reg rd)
	fun arith3_ud(rd,rt,rs)   = 
	  (mem::alloc::exnptr::(reg rt @ reg rs), reg rd)
	fun logical3_ud(rd,rt,rs) = (reg rs @ reg rt, reg rd)

        fun double_float_ud(fd,fs,ft) =
	  (fregd fs @ fregd ft, fregd fd)

	fun mult_ud(rt,rs)        = 
	  (reg rs @ reg rt, [LO,HI])

	fun load_ud(rt,base,_)    = (mem::npc::anyreg base,anyreg rt)
	fun store_ud(rt,base,_)   = let val use = anyreg rt @ anyreg base
				    in if is_allocR base then (use, []) 
				       else (use,[mem])
				    end
    end
in

  fun rUseDef inst =
      case inst 
      of M.NOP            => ([], [])

       | M.BEQ(_,rt,rs,_) => (RId.reg rs @ RId.reg rt,   [RId.npc])
       | M.JUMP rs        => (RId.reg rs, 	         [RId.npc])
       | M.BCOP1 _        => ([RId.fcc], 	         [RId.npc])
       | M.BREAK _        => (allR,			 allR)

       | M.SLT arg           => logical_ud arg
       | M.SLTU arg	     => logical_ud arg
       | M.FCMP(_,rt,rs)     => (RId.fregd rt @ RId.fregd rs, [RId.fcc]) 

       | M.AND arg => logical_ud arg
       | M.OR arg  => logical_ud arg
       | M.XOR arg => logical_ud arg

       | M.ADD(arg as (rd,_,rs))  => let
	   val (u,d) = arith_ud arg
	 in if is_allocR rd then (u, RId.mem::d) else (RId.alloc::u, d)
	 end
       | M.ADDU arg => logical_ud arg
       | M.SUB arg  => arith3_ud arg
       | M.SUBU arg => logical3_ud arg
			
       | M.MFLO rd => ([RId.LO], RId.reg rd)
       | M.MFHI rd => ([RId.HI], RId.reg rd)

       | M.MULT arg => mult_ud arg
       | M.MULTU arg=> mult_ud arg
       | M.DIV arg  => mult_ud arg
       | M.DIVU arg => mult_ud arg

       | M.NEG_DOUBLE(fd,fs)	 => (RId.fregd fs, RId.fregd fd)
       | M.ABS_DOUBLE(fd,fs)	 => (RId.fregd fs, RId.fregd fd)
       | M.MUL_DOUBLE arg    	 => double_float_ud arg
       | M.DIV_DOUBLE arg    	 => double_float_ud arg
       | M.ADD_DOUBLE arg    	 => double_float_ud arg 
       | M.SUB_DOUBLE arg    	 => double_float_ud arg 
       | M.CVTI2D(dst,src) 	 => (RId.freg src,RId.fregd dst)
       | M.MTC1(src,dst)         => (RId.reg src,RId.freg dst)
	     
       | M.MOV_DOUBLE(fd,fs) =>	(RId.fregd fs, RId.fregd fd)

       | M.LBU arg        => load_ud arg
       | M.LW arg         => load_ud arg
       | M.LWC1 arg       => load_ud arg
       | M.SB arg         => store_ud arg
       | M.SW arg         => store_ud arg
       | M.SWC1 arg	  => store_ud arg

       | M.LUI(rd,_)      => ([], RId.reg rd)
		       
       | M.SLL(rd,rt,_)   => (RId.reg rt, RId.reg rd)
       | M.SRA(rd,rt,_)   => (RId.reg rt, RId.reg rd)
       | M.SRL(rd,rt,_)   => (RId.reg rt, RId.reg rd)
       | M.SLLV arg 	  => logical3_ud arg
       | M.SRAV arg 	  => logical3_ud arg
       | M.SRLV arg 	  => logical3_ud arg


  fun usesReg (r,I) = let val (ul,_) = rUseDef I
			  val [rR] = RId.anyreg r
		      in List.exists (fn u => u = rR) ul
		      end

  fun mayNeedNop (M.LBU _)        = 1
    | mayNeedNop (M.LW _)         = 1
    | mayNeedNop (M.LWC1 _)       = 1
    | mayNeedNop (M.FCMP _)       = 1
    | mayNeedNop (M.MFLO _)       = 2
    | mayNeedNop (M.MFHI _)       = 2
    | mayNeedNop (M.MTC1 _)	  = 1
    | mayNeedNop (M.BEQ _)	  = 1
    | mayNeedNop (M.BCOP1 _)	  = 1
    | mayNeedNop (M.JUMP _)	  = 1
    | mayNeedNop _ 		  = 0

  fun multop (MULT _) = true
    | multop (MULTU _) = true
    | multop (DIV _) =  true
    | multop (DIVU _) =  true
    | multop _ = false

  fun needsNop(instr, MFLO _::_) =  if multop instr then 2 else 0
    | needsNop(instr, _::MFLO _::_) = if multop instr then 1 else 0
    | needsNop(instr, MFHI _::_) =  if multop instr then 2 else 0
    | needsNop(instr, _::MFHI _::_) = if multop instr then 1 else 0
    | needsNop(next,prev::rest) =
      (case prev
	of M.LBU(rd,_,_)  => if usesReg(rd,next) then 1 else 0
	 | M.LW(rd,_,_)   => if usesReg(rd,next) then 1 else 0
	 | M.LWC1(fd,_,_) => if usesReg(fd,next) then 1 else 0
	 | M.FCMP _       => (case next of M.BCOP1 _ => 1 | _ => 0)
	 | M.MTC1(_,fs)   => if usesReg(fs,next) then 1 else 0
	 | _          => (case (instrKind next,instrKind prev) 
			    of (IK_JUMP,IK_JUMP) => 1
			     | _  => 0))
    | needsNop _ = 0

  (* R2000 latencies *)
  fun latency (M.LW _) 		= 2
    | latency (M.LWC1 _) 	= 2
    | latency (M.LBU _)		= 2
    | latency (M.MULT _)	= 11
    | latency (M.MULTU _)	= 11
    | latency (M.DIV _)		= 36
    | latency (M.DIVU _)	= 36
    | latency (M.JUMP _)	= 2
    | latency (M.BEQ _)		= 2
    | latency (M.BCOP1 _)	= 2
    | latency (M.FCMP _)        = 2
    | latency (M.ADD_DOUBLE _)  = 2
    | latency (M.MUL_DOUBLE _)  = 5
    | latency (M.DIV_DOUBLE _)	= 19
    | latency _			= 1

end 
end

(*
 * $Log: mipsdepend.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:47  george
 * Version 110.5
 *
 *)
