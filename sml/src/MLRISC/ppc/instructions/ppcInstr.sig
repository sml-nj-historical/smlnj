(* ppcInst.sig -- power pc instruction set *)

(*
 * COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs 
 *)
signature PPCINSTR = sig
  structure Constant : CONSTANT
  structure Region   : REGION  
  structure C : PPCCELLS 
    where type cellset = int list * int list * int list

  type gpr = int			(* general purpose register *)
  type fpr = int			(* floating point register *)
  type ccr = int			(* condition code register *)
  type crf = int			(* condition register field *)

  datatype spr = XER | LR | CTR

  datatype operand = 
      RegOp of int
    | ImmedOp of int
    | LabelOp of LabelExp.labexp
    | ConstOp of Constant.const

  datatype ea = 
      Direct of int
    | FDirect of int
    | Displace of {base:int, disp:operand} (* RegOp illegal as operand *)

  datatype size = Byte | Half | Word | Long | Single | Double

  datatype cmp = CMP | CMPL
  datatype fcmp = FCMPO (* ordered *) | FCMPU	(* unordered *)
  datatype unary = NEG
  datatype funary = FMR | FABS | FNEG
  datatype farith  = FADD | FSUB | FMUL | FDIV
  datatype bo = 
      TRUE				(* 011zy *)
    | FALSE				(* 001zy *)
    | ALWAYS				(* 1z1zz *)
    | COUNTER of {eqZero:bool, cond:bool option}
		        (* operation			ARITH	ARITHI *)
  datatype arith =      (* ---------			-----	------ *) 
      ADD		(* add				add     addi   *)
    | ADDS		(* add-shifted			 -	addis  *)
    | SUBF		(* subtract from		subf	subfic *)
    | MULL		(* multiply			mullw   mulli  *)
    | DIVW		(* divide word			divw      -    *)
    | DIVWU		(* divide word unsigned		divwu     -    *)
    | AND		(* and				and	andi   *)
    | OR		(* or				or	ori    *)
    | XOR		(* xor				xor	xori   *)
    | XORS		(* xor shifted			 -	xoris  *)
    | SLW		(* shift left word		slw	rlwinm *)
    | SRW		(* shift right word		srw     rlwinm *)
    | SRAW		(* shift right algebrai word	sraw	srawi  *)

  datatype rotate = 
      RLWNM		(* rotate left word AND mask	rlwnm	rlwinm *)

  datatype ccarith =
      CRAND				(* cond. reg. AND *)
    | CROR				(* cond. reg. OR *)
    | CRXOR				(* cond. reg. XOR *)
    | CRNAND				(* cond. reg. NAND *)
    | CRNOR				(* cond. reg. NOR *)
    
  (* bits in condition code *)
  datatype bit = 
      LT | GT  | EQ | SO		(* cr0 *)
    | FL | FG  | FE | FU		(* cr1 *)
    | FX | FEX | VX | OX		

  type cr_bit = int * bit

  datatype instruction =
      L of {sz:size, rt:int, ra:int, d:operand, mem:Region.region}
    | ST of {sz:size, rs:int, ra:int, d:operand, mem:Region.region}
    | UNARY of {oper:unary, rt:int, ra:int, Rc:bool, OE:bool}
    | ARITH of {oper:arith, rt:int, ra:int, rb:int, Rc:bool, OE:bool}
    | ARITHI of {oper:arith, rt:int, ra:int, im:operand}
    | ROTATE of {oper:rotate, ra:int, rs:int, sh:operand, mb:int, me: int}
    | COMPARE of {cmp:cmp, bf: int, ra:int, rb:operand}
    | FCOMPARE of {cmp:fcmp, bf:int, fa:int, fb:int}

    | FUNARY of {oper:funary, ft:int, fb:int, Rc:bool}
    | FARITH of {oper:farith, ft:int, fa:int, fb:int, Rc:bool}

    | CCARITH of {oper:ccarith, bt:cr_bit, ba:cr_bit, bb:cr_bit}
    | MCRF of {bf:int, bfa:int}

    (* Special Purpose Registers *)
    | MTSPR of {rs:int, spr:int}
    | MFSPR of {rt:int, spr:int}

    (* Trapping Instructions *)
    | TWI of {to:int, ra:int, si:operand}

    (* Control Instructions -  AA is always assumed to be 0 *)
    | BC   of {bo:bo, bf:int, bit:bit, addr:operand, LK:bool, fall:operand}
    | BCLR of {bo:bo, bf:int, bit:bit, LK:bool, labels:Label.label list}
    | B    of {addr:operand, LK:bool}

    (* CALL = BCLR {bo=ALWAYS, bf=0, bit=0, LK=true, labels=[] *)
    | CALL of {def:C.cellset, use:C.cellset}

    | COPY of {dst:int list, src: int list, impl:instruction list option ref, 
	       tmp: ea option}
    | FCOPY of {dst: int list, src: int list, impl:instruction list option ref,
		tmp: ea option}

  val mtlr : int -> instruction
  val mflr : int -> instruction
  val ret : unit -> instruction
end

