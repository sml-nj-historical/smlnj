(* rs6000instrset.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** IBM RS6000 Instruction set *)
structure RS6000InstrSet = struct

  fun error msg = ErrorMsg.impossible ("RS6KInstrSet." ^ msg)

  datatype 'lab info = INFO of {addrOf: 'lab -> int, nameOf: 'lab -> string}
(**
  abstype register = REGISTER of int 
  		     with
			 datatype register_rep = Reg' of int | Freg' of int
			 fun Reg i = REGISTER i
			 fun Freg i = REGISTER(i+32)
			 fun reg_rep (REGISTER i) = if i<32 then Reg' i
						    else Freg'(i-32)
			 fun reg_eq(REGISTER r,REGISTER s) =  r=s
		     end
**)
  datatype register = Reg of int | Freg of int
  datatype specialReg = LR | MQ
  datatype 'label EA  = Direct of register
		      | Immed of int
		      | Immed32 of Word32.word
		      | ImmedLab of 'label

  datatype 'label signedlabel = POSLAB of 'label | NEGLAB of 'label
  type 'label labexp = 'label signedlabel * int

  datatype 'label eaOpnd = Immed16Op of int
	                 | RegOp of register
   	   	         | LabelOp of 'label labexp
	  	         | HiLabOp of 'label labexp
 		         | LoLabOp of 'label labexp

  datatype 'label branch24Off = Label24Off of 'label labexp

  datatype 'label branch16Off = Label16Off of 'label labexp

  datatype shamt = RegShift of register
		 | Int5Shift of int

 (* Note: the limitReg is hardwired into runtime/signal.c *)
  val stackReg			= Reg 1
  val allocReg  		= Reg 14
  val limitReg  		= Reg 15
  val exnptrReg 		= Reg 21
  val baseReg 			= Reg 23
  val maskReg			= Reg 29
  val constBaseRegOffset	= 32764
  val fLoadStoreOff		= 24   (* rely on RS6000.prim.asm *)

  datatype condition = LT | GT | EQ | SO   (* cr0 *)
		     | FL | FG | FE | UN   (* cr1 *)
    		     | FX | FEX | VX | OX  (* cr2 *)

  datatype 'label sdi =
        SETBASEADDR of 'label * register
      | LOADADDR    of register * 'label * int
      | LOAD        of register * 'label * int 
      | LOADF       of register * 'label * int * register
      | BRANCH      of condition * bool * 'label * register * 'label
      | FBRANCH     of condition * int * bool * 'label * register * 'label

  fun split n = let 
    val hi = Word.~>>(Word.fromInt n, 0w16) 
    val lo = Word.andb(Word.fromInt n, 0w65535)
  in 
    if lo < 0w32768 then (hi,lo) else (hi+0w1, lo-0w65536)
  end

  fun labelValue (INFO{addrOf,...}) (POSLAB lab,k) = k + addrOf lab
    | labelValue (INFO{addrOf,...}) (NEGLAB lab,k) = k - addrOf lab

  fun hiLabelValue info labexp = #1 (split (labelValue info labexp))
  fun loLabelValue info labexp = #2 (split (labelValue info labexp))

  local 
      fun labelVal(info,labexp) = let exception  BranchOffset
				      val labOff = labelValue info labexp
				  in
				      labOff div 4
				  end
  in
    fun labBranch24Off info (Label24Off labexp) = labelVal(info,labexp)
    fun labBranch16Off info (Label16Off labexp) = labelVal(info,labexp)
  end

  datatype 'l instruction =
      NOP

    | B     of 'l branch24Off			 (* long branch *)
    | BB    of condition * bool * 'l branch16Off	
    | BBF   of condition * int  * bool * 'l branch16Off   
    | BR    of unit				 (* branch via link reg *)

    | LBZ   of register   * register * 'l eaOpnd (* load byte and zero *)
    | L	    of register   * register * 'l eaOpnd (* load word *)
    | LFD   of register   * register * 'l eaOpnd (* load float *)
    | LIU   of register   * 'l eaOpnd		 (* load immediate upper *)

    | STB   of register   * register * 'l eaOpnd
    | ST    of register   * register * 'l eaOpnd (* store word *)
    | STFD  of register   * register * 'l eaOpnd (* store floating double *)

    | FMR   of register   * register		 (* floating move *)
    | MTSPR of specialReg * register		 (* move to special reg *)
    | CAL   of register * register * 'l eaOpnd	 (* calculate address lower *)

    | A	    of register * register * 'l eaOpnd	 (* Add w/o overflow detection *)
    | AO    of register * register * register    (* Add w overflow detection *)
    | FAO   of register * register * register	 (* floating add w overflow *)

						 (** Subtract **)
    | SF    of register * register * 'l eaOpnd	 (* sub from w/o overflow *)
    | SFO   of register * register * register	 (* sub from w. overflow *)
    | FSO   of register * register * register	 (* float sub from w. overflow *)

    | MULS  of register * register * register    (* mult w/o overflow *)
    | MULSO of register * register * register    (* mult  w overflow *)
    | FMO   of register * register * register    (* floating mult w overflow *)

    | DIV   of register * register * register    (* div w/o overflow *)
    | DIVS  of register * register * register	 (* div w. overflow *)
    | FDO   of register * register * register    (* floating div. w overflow *)

    | FNEG  of register * register		 (* floating negate *)
    | FABS  of register * register		 (* floating absolute *)

						 (* comparisons *)
    | CMP   of register * 'l eaOpnd		 (* compare signed *)
    | CMPL  of register * register		 (* compare logical (unsigned) *)
    | FCMP  of register * register		 (* floating compare *)
    | CROR  of condition * condition * condition (* cr bit field OR *)

						 (* logical *)
    | AND   of register * register * 'l eaOpnd   (* and *)
    | OR    of register * register * 'l eaOpnd	 (* or *)
    | XOR   of register * register * 'l eaOpnd	 (* xor *)
    | XORU  of register * register * 'l eaOpnd	 (* xor upper *)

						 (* shift *)
    | SL    of register * register * shamt	 (* shift left *)
    | SRA   of register * register * shamt	 (* shift right *)
    | SRL   of register * register * shamt	 (* shift right logical *)

    | MTFSB1 of int				 (* set bit in FPSCR *)
    | TRAP of unit				 (* trap always *)
end


(*
 * $Log$
 *)
