(* mipsinstr.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure MipsInstrSet = 
struct

  val error = ErrorMsg.impossible

  datatype 'label info = INFO of {addrOf: 'label -> int, 
				    nameOf: 'label->string}
  (*
   * satisfy ea requirements of cmachine
   *)
  datatype register_rep = Reg' of int | Freg' of int
  abstype register = REGISTER of int
  with
       fun Reg i = REGISTER i
       fun Freg i = REGISTER(i+32)
       fun reg_rep (REGISTER i) = if i<32 then Reg' i else Freg' (i-32)
       fun reg_eq(REGISTER r, REGISTER s) = r=s
  end

  datatype 'label EA = Direct of register
		    | Immed of int
 		    | ImmedLab of 'label
		    | Immed32 of Word32.word

  val baseReg   = Reg 24
  val limitReg  = Reg 19
  val heapExhaustedReg = Reg 21
  val maskReg = Reg 25
  val allocReg  = Reg 23
  val exnptrReg = Reg 30
  val linkReg   = Reg 31

  datatype 'label sdi  
    = SETBASEADDR of 'label * register
    | LOADADDR    of register * 'label * int
    | LOAD        of register * 'label * int
    | LOADF       of register * 'label * int * register
    | BRANCH      of bool * register * register * 'label * register * 'label
    | BRANCH_COP1 of bool * 'label * register * 'label

  val constBaseRegOffset = 32764

 (* satisfy operand requirements of mips instruction set.
  * memOpnd should require no more than 16 bits when computed.
  *)
  datatype 'label signedlabel = POSLAB of 'label | NEGLAB of 'label
  type 'label labexp = 'label signedlabel * int
  datatype 'label arithOpnd = Immed16Op of int 
		     	    | RegOp of register
		     	    | LabelOp of 'label labexp
		     	    | HiLabOp of 'label labexp
		     	    | LoLabOp of 'label labexp

  datatype 'label memOpnd = Immed16Off of int
			  | LabOff of 'label labexp 
			  | HiLabOff of 'label labexp
		   	  | LoLabOff of 'label labexp

  datatype int5      = Int5 of int

  datatype fcond = UN			(* ? *)
		 | EQ			(* = *)
		 | UEQ			(* ?= *)
		 | OLT			(* NOT ?>= *)
		 | ULT			(* ?< *)
		 | OLE			(* NOT ?> *)
		 | ULE			(* ?<= *)
		 | NGLE			(* NOT <=> *)
		 | NGL			(* NOT <> *)
		 | LT			(* < *)
		 | NGE			(* NOT >= *)
	         | LE			(* <= *)
	         | NGT			(* NOT > *)

  fun split i = let
    val hi = Word.~>>(Word.fromInt i, 0w16) 
    val lo = Word.andb(Word.fromInt i, 0w65535)
  in 
    if lo < 0w32768 then (hi,lo) else (hi+0w1, lo-0w65536)
  end

  fun chk_immed16 n =  let
	exception Immed16
      in  if n>= ~32768 andalso n<=65535 then Word.fromInt n 
	  else (app Control.Print.say
		  ["Immed16 constant too large ", Int.toString n, "\n"];
		raise Immed16)
      end

  fun labelValue (INFO{addrOf,...}) (POSLAB lab,k) = k + addrOf lab
    | labelValue (INFO{addrOf,...}) (NEGLAB lab,k) = k - addrOf lab

  fun hiLabelValue info labexp = #1 (split (labelValue info labexp))
  fun loLabelValue info labexp = #2 (split (labelValue info labexp))

  fun labBranchOff info (LabOff labexp) = let 
	exception BranchOffset
	val labOff = labelValue info labexp
      in 
	  if labOff mod 4 <> 0 then raise BranchOffset
	  else labOff div 4
      end
    | labBranchOff _ _ = error "MipsInstrSet.labBranchOff: bad label"

  datatype 'label instruction 
    = NOP

   (*
    * compare instructions:
    *)
    | SLT  	of register * register * 'label arithOpnd
    | SLTU 	of register * register * 'label arithOpnd
    | FCMP      of fcond * register * register

   (*
    * branch instructions.
    * This list is incomplete as the other branch instructions
    * are all span dependent.
    *)
    | JUMP 	of register
    | BEQ	of bool * register * register * 'label memOpnd
    | BCOP1     of bool * 'label memOpnd


   (* 
    * Arithmetic instructions:
    * arguments are (rd,rs,rt/immed) with the exception of sub (sigh).
    *)
    | ADD 	of register * register * 'label arithOpnd
    | ADDU	of register * register * 'label arithOpnd
    | AND 	of register * register * 'label arithOpnd
    | OR 	of register * register * 'label arithOpnd
    | XOR 	of register * register * 'label arithOpnd
    | SUB 	of register * register * register
    | SUBU	of register * register * register
    (* 
     * integer mult and div related:
     *)
    | MULT 	of register * register
    | MULTU	of register * register
    | DIV 	of register * register 
    | DIVU      of register * register
    | MFLO 	of register
    | MFHI 	of register
    | BREAK	of int

   (* 
    * Floating point arithmetic:
    *)
    | NEG_DOUBLE  of register * register 
    | ABS_DOUBLE  of register * register 
    | MUL_DOUBLE  of register * register * register 
    | DIV_DOUBLE  of register * register * register 
    | ADD_DOUBLE  of register * register * register 
    | SUB_DOUBLE  of register * register * register 
    | CVTI2D      of register * register
    | MTC1	  of register * register

   (* 
    * Move pseudo-instruction :  move(dst,src)
    *)
    | MOV_DOUBLE  of register * register 

   (* Load and store instructions:
    * arguments are: op(rd, base(offset))
    *)
    | LBU  	of register * register * 'label memOpnd
    | SB  	of register * register * 'label memOpnd
    | LW  	of register * register * 'label memOpnd
    | SW 	of register * register * 'label memOpnd
    | LWC1	of register * register * 'label memOpnd
    | SWC1	of register * register * 'label memOpnd
    | LUI 	of register * 'label memOpnd

   (* 
    * Shift instructions:
    * arguments are: (rd,rs,shamt)
    *)
    | SLL of register * register * int5
    | SLLV of register * register * register
    | SRA of register * register * int5
    | SRAV of register * register * register 
    | SRL of register * register * int5
    | SRLV of register * register * register 

   (* MIPS-2 instructions (i.e., R4000, R6000) *)
    | LDC1	of register * register * 'label memOpnd
    | SDC1	of register * register * 'label memOpnd

end    

  


(*
 * $Log: mipsinstr.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:48  george
 * Version 110.5
 *
 *)
