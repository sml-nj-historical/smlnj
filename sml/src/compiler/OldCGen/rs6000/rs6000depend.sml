(* rs6000depend.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** IBM RS6000 specific dependences **)

structure RS6000Depend : MACHINSTR = struct
  structure M =  RS6000InstrSet
  open M

  fun error msg = ErrorMsg.impossible ("RS6kDepend." ^ msg)

  val branchDelayedArch = false
 
  datatype ikind = IK_NOP | IK_JUMP | IK_INSTR

  fun isSdiBranch (M.BRANCH _)  = true
    | isSdiBranch (M.FBRANCH _) = true
    | isSdiBranch _ 		= false
  
  fun instrKind instr = 
      case instr of 
	   M.NOP   => IK_NOP
	 | M.B _   => IK_JUMP
	 | M.BB _  => IK_JUMP
	 | M.BBF _ => IK_JUMP
	 | M.BR _  => IK_JUMP
	 | _       => IK_INSTR

  val nop = M.A(Reg 0,Reg 0,RegOp(Reg 0))

  fun minSize _ = 4

  fun sizeOf (info as INFO{addrOf,...}) (sdi,loc) = let
      fun lab_value (lab,k) = M.labelValue info 
	  				   (POSLAB lab,k-M.constBaseRegOffset)
      fun is_short n = ~32768 <= n andalso n <= 32767
      fun is_short_branch lab = let val displ = (addrOf lab - loc) div 4
	in displ >= ~8192 andalso displ < 8192
        end
    in
	case sdi
	  of M.SETBASEADDR(lab,reg) => let
  	       val const = M.constBaseRegOffset - addrOf lab
	     in 
		 if is_short const then (false, 4) else (true, 12)
  	     end
	   | M.LOADADDR(_,lab,k) => 
	     if is_short(lab_value(lab,k)) then (false,4) else (true,12) 
           | M.LOAD(_,lab,k) =>
	     if is_short(lab_value(lab,k)) then (false,4) else (true,12) 
           | M.LOADF(_,lab,offset,_)  => let
  	       val labexp1 = (POSLAB lab,offset-M.constBaseRegOffset)
	       val labexp2 = (POSLAB lab,offset-M.constBaseRegOffset+4)
   	       val labval1 = M.labelValue info labexp1
	       val labval2 = M.labelValue info labexp2
             in
		 if (is_short labval1 andalso is_short labval2) 
		 then (false,20)
		 else (true,32)
             end
           | M.BRANCH(_,_,lab,_,_) =>
 	     if is_short_branch lab then (false,4) else (true,24)
           | M.FBRANCH(_,_,_,lab,_,_) =>
  	     if is_short_branch lab then (false,4) else (true,24)
  end

  fun expand _ (sdi,size,loc) = 
      case sdi of
	M.SETBASEADDR(lab,reg) => 
	  let val labexp = (M.NEGLAB lab,M.constBaseRegOffset)
	  in case size of
		4  => 
		    [M.A(M.baseReg,reg,M.LabelOp labexp)]
	      | 12 => 
		    [M.LIU(M.baseReg,M.HiLabOp labexp),
		     M.A(M.baseReg,M.baseReg,M.LoLabOp labexp),
		     M.A(M.baseReg,reg,RegOp M.baseReg)]
	      | _  => error "expand: SETBASEADDR"
	  end
      | M.LOADADDR(r,lab,k) => 
	   let val labexp = (POSLAB lab,k-M.constBaseRegOffset)
	   in case size
	      of 4  => 
		     [M.A(r,M.baseReg,LabelOp labexp)]
	       | 12 =>  
		     [M.LIU(r,HiLabOp labexp),
		      M.A(r,r,LoLabOp labexp),
		      M.A(r,M.baseReg,RegOp r)]
	       | _  => error "expand: LOADADDR"
	   end
      | LOAD(r,lab,k) => 
	   let val labexp = (POSLAB lab,k-M.constBaseRegOffset)
	   in case size
	      of 4  => [M.L(r,M.baseReg,LabelOp labexp)]
	       | 12 => [M.LIU(r,HiLabOp labexp),
			M.A(r,M.baseReg,RegOp r),
			M.L(r,r,LoLabOp labexp)]
	       | _ => error "expand: LOAD"
	   end
      | M.LOADF(Freg fp,lab,offset,tmpR) => 
	   let val labexp1 = (POSLAB lab,offset-M.constBaseRegOffset)
	       val labexp2 = (POSLAB lab,offset-M.constBaseRegOffset+4)
	       val stackptr = Reg 1
	   in case size 
		of 20  => 
		       [M.L(Reg 0,M.baseReg,LabelOp labexp1),
			M.L(tmpR,M.baseReg,LabelOp labexp2),
			M.ST(Reg 0,stackptr,Immed16Op M.fLoadStoreOff),
			M.ST(tmpR,stackptr,Immed16Op(M.fLoadStoreOff+4)),
			M.LFD(Freg fp,stackptr,Immed16Op M.fLoadStoreOff)]
	         | 32 => 
		       [M.LIU(tmpR,HiLabOp labexp1),
			M.A(tmpR,tmpR,RegOp M.baseReg),
			M.A(tmpR,tmpR,LoLabOp labexp1),
			M.L(Reg 0,tmpR,Immed16Op 0),
			M.ST(Reg 0,stackptr,Immed16Op(M.fLoadStoreOff)),
			M.L(Reg 0,tmpR,Immed16Op 4),
			M.ST(Reg 0,stackptr,Immed16Op(M.fLoadStoreOff+4)),
			M.LFD(Freg fp,stackptr,Immed16Op M.fLoadStoreOff)]
	         | _ => error "expand: LOADF"
	   end
      | M.BRANCH(cc,bool,tlab,tmpR,flab) => 
	   (case size
	    of 4  => [M.BB(cc,bool,Label16Off(POSLAB tlab,0))]

 	     | 24 => 
		  let val labexp = (POSLAB tlab,~M.constBaseRegOffset)
		  in
		      [M.BB(cc,not bool,Label16Off(POSLAB flab,0)),

		       (** Could use a branch24 **)
		       M.LIU(tmpR,HiLabOp labexp),
		       M.A(tmpR,tmpR,LoLabOp labexp),
		       M.A(tmpR,tmpR,RegOp M.baseReg),
		       M.MTSPR(M.LR,tmpR),
		       M.BR ()]
		  end
	     | _ => error "expand: BRANCH")
     | M.FBRANCH(cc,cr,bool,tlab,tmpR,flab) =>
	   (case size 
              of 4 => [M.BBF(cc,cr,bool,Label16Off (POSLAB tlab,0))]
	       | 24 => 
		     let val labexp = (POSLAB tlab,~M.constBaseRegOffset)
		     in
			 [M.BBF(cc,cr,not bool,Label16Off(POSLAB flab,0)),

			  M.LIU(tmpR,HiLabOp labexp),
			  M.A(tmpR,tmpR,LoLabOp labexp),
			  M.A(tmpR,tmpR,RegOp M.baseReg),
			  M.MTSPR(M.LR,tmpR),
			  M.BR ()]
		     end
	       | _ => error "expand: FBRANCH")
     | _ => error "expand"


 (*
  * Resources: ($0-$31)+($f0,$f1-$f31)+cc+fcc+npc+LR+mem+fpscr+stack+MQ
  *)

  val numResources = 72

  structure RId = struct
     fun reg (Reg r) = [r] | reg _ = error "RId.reg"
     fun fregd (Freg i) = [32 + i] | fregd _ = error "RId.fregd"
     fun anyreg r = case r of Reg _ => reg r | Freg _ => fregd r
     val cc  	= 64
     val fcc 	= 65 
     val npc	= 66 
     val lr  	= 67
     val mem 	= 68
     val fpscr 	= 69
     val stack  = 70
     val mq     = 71

     val Reg(alloc)    = M.allocReg (* resource no. for reg = reg no. *)
     val Reg(exnptr)   = M.exnptrReg
     val Reg(stackptr) = M.stackReg
  end

  local 
    open RId
    fun is_allocR reg = reg = M.allocReg
    fun is_stackR reg = reg = M.stackReg
    fun arithOpndUse opnd = case opnd of RegOp r => RId.reg r | _ => []

    val allR = let fun f(~1,l) = l | f(i,l) = f(i-1,i::l)
	       in f(numResources-1,[])
	       end

    fun arith_ud(rd,rs,ea)    = (reg rs @ arithOpndUse ea, reg rd)
    fun arith_3reg_ud(rd,ra,rb)= (reg ra @ reg rb,reg rd)
    fun logical_ud(rd,rs,ea)  = (reg rs @ arithOpndUse ea, cc::reg rd)
    fun logical_3reg_ud(rd,ra,rb)  
			      = (reg ra @ reg rb, cc::reg rd)
    fun double_cc_ud(fd,fs,ft)= (fregd fs @ fregd ft, cc::fregd fd)

    fun load_ud(rt,base,ea)   = let val u = anyreg base @ arithOpndUse ea
				    val d = anyreg rt
				in 
				    if is_stackR base then (stack::u,d)
				    else (mem::u,d)
				end
    fun store_ud(rt,base,ea)  = let val use = anyreg rt @ anyreg base @ 
					      arithOpndUse ea
				in if is_stackR base then (use,[stack])
				   else if is_allocR base then (use, []) 
					else (use,[mem])
				end
    fun shift_ud(ra,rs,sh)    = let val (u,d) = (reg rs, reg ra)
				in case sh 
				     of RegShift sh => (reg sh @ u, d)
				      | _ => (u,d)
				end
  in
    fun rUseDef I = 
	case I 
        of NOP 	  		=> error "rUseDef: NOP"

         | M.B _ 		=> ([],[npc])
	 | M.BB _ 		=> ([lr,cc], [npc])
	 | M.BBF _ 		=> ([lr,fcc],[npc])
	 | M.BR _ 		=> ([lr],[npc])

	 | M.LBZ arg 		=> load_ud arg
	 | M.L arg 		=> load_ud arg
	 | M.LFD arg 		=> load_ud arg
	 | M.LIU (rt,_) 	=> ([],RId.reg rt)
	 | M.MTSPR(M.LR,ra) 	=> (RId.reg ra,[lr])
	 | M.MTSPR(M.MQ,ra)	=> (RId.reg ra,[mq])
	 | M.CAL arg    	=> arith_ud arg

	 | M.STB arg 		=> store_ud arg
	 | M.ST	arg  		=> store_ud arg
	 | M.STFD arg 		=> store_ud arg

         | M.A(arg as (Reg rd,_, _)) => let
             val (u,d) = arith_ud arg
	   in
	       if is_allocR (Reg rd) then (u,mem::d) else (u,d)
           end  
	 | M.A _		=> error "rUseDef: M.A"
	 | M.AO arg 		=> logical_3reg_ud arg
	 | M.FAO arg 		=> double_cc_ud arg

	 | M.SF arg 		=> arith_ud arg
	 | M.SFO arg 		=> logical_3reg_ud arg
	 | M.FSO arg 		=> double_cc_ud arg

	 | M.MULSO arg 		=> logical_3reg_ud arg
	 | M.MULS arg		=> arith_3reg_ud arg
	 | M.FMO arg 		=> double_cc_ud arg

	 | M.DIVS arg 		=> logical_3reg_ud arg
	 | M.DIV arg		=> arith_3reg_ud arg
  	 | M.FDO arg 		=> double_cc_ud arg

	 | M.FNEG(ra,rb)	=> (RId.fregd rb, fcc::RId.fregd ra)
	 | M.FABS(frt,frb)	=> (RId.fregd frb, RId.fregd frt)

      	 | M.CMP(ra,ea) 	=> (reg ra @ arithOpndUse ea, [cc])
	 | M.CMPL(ra,rb)	=> (reg ra @ reg rb, [cc])
	 | M.FCMP(fra,frb) 	=> (fregd fra @ fregd frb, [fcc])
	 | M.CROR _		=> ([fcc], [fcc])

         | M.AND arg 		=> logical_ud arg
	 | M.OR arg 		=> logical_ud arg
	 | M.XOR arg 		=> logical_ud arg
	 | M.XORU arg 		=> logical_ud arg

	 | M.SL arg 		=> shift_ud arg
	 | M.SRA arg 		=> shift_ud arg
	 | M.SRL arg		=> shift_ud arg

	 | M.FMR(frt,frb) 	=> (RId.fregd frb, RId.fregd frt)
         | M.MTFSB1 n     	=> ([], [fpscr])
	 | M.TRAP() 		=> ([fpscr,exnptr,alloc], [npc])
  end

  fun latency (M.LBZ _) 	= 2
    | latency (M.L _)		= 2
    | latency (M.LFD _)		= 2
    | latency (M.CMP _)		= 3
    | latency (M.CMPL _)	= 3
    | latency (M.FCMP _)	= 8
    | latency (M.MTSPR _)	= 4
    | latency (M.FAO _)		= 2
    | latency (M.FSO _)		= 2
    | latency (M.FMO _)		= 2
    | latency (M.FDO _)		= 2
    | latency _			= 1

  fun mayNeedNop _ = 0
 
  fun needsNop _   = 0
end 


(*
 * $Log: rs6000depend.sml,v $
 * Revision 1.4  1997/11/14 21:48:09  jhr
 *   Restored the support for the Power architecture; the PowerPC code
 *   generator will be MLRisc based.
 *
 * Revision 1.3  1997/08/25  16:43:33  jhr
 *   Replaced some old Power architecture instructions with PowerPC instructions.
 *   This means that the Power architecture is no longer supported by this
 *   code generator.  Also improved implementation of emitString.
 *
 * Revision 1.2  1997/05/05  19:58:02  george
 *   Add the allocation pointer to the list of source registers for the
 *   tvs instruction. This fixed the subtle bug on interactions between
 *   the allocation-pointer-adjustment instruction and the addi
 *   instruction. -- george
 *
 * Revision 1.1.1.1  1997/01/14  01:38:45  george
 *   Version 109.24
 *
 *)
