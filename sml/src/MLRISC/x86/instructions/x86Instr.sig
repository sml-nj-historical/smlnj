(* x86Instr.sml -- 32bit, x86 instruction set.
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *
 *
 *)
signature X86INSTR = sig
  type csets = int list * int list * int list

  structure C : X86CELLS where type cellset = csets
  structure Constant : CONSTANT
  structure Region : REGION

 (* An effective address can be any combination of
  *  base + index*scale + disp 
  *   or
  *   B + I*SCALE + DISP
  *
  * where any component is optional. The operand datatype captures
  * all these combinations.
  *
  *  DISP 	    == Immed | ImmedLabel | Const
  *  B		    == Displace{base=B, disp=0}
  *  B+DISP	    == Displace{base=B, disp=DISP}
  *  I*SCALE+DISP   == Indexed{base=NONE, index=I, scale=SCALE, disp=D}
  *  B+I*SCALE+DISP == Indexed{base=SOME B, index=I, scale=SCALE, disp=DISP}
  *
  *  Note1: The index register cannot be EBP.
  *         The disp field must be one of Immed, ImmedLabel,  or Const.
  *)

  (* Note: Relative is only generated after sdi resolution *)
  datatype operand =
     Immed      of Int32.int
   | Const      of Constant.const
   | ImmedLabel of LabelExp.labexp
   | Relative   of int			
   | LabelEA	of LabelExp.labexp
   | Direct     of int
   | FDirect    of int
   | Displace   of {base:int, disp:operand}
   | Indexed    of {base:int option, index:int, scale:int, disp:operand}
  
  type ea = operand

  datatype binaryOp = ADD | SUB  | AND | OR | XOR | SHL | SAR | SHR

  datatype multDivOp = UMUL | IDIV | UDIV

  datatype unaryOp = DEC | INC | NEG | NOT

  datatype move = MOVL | MOVZX | MOVB

  datatype cond = 
      EQ | NE | LT | LE | GT | GE 
    | B  | BE (* below *)   | A  | AE (* above *) 
    | C  | NC (* if carry *)| P  | NP (* if parity *)
    | O  | NO (* overflow *) 

 (* The Intel manual is incorrect on the description of FDIV and FDIVR *)
  datatype fbinOp = 
      FADDP  | FADD 
    | FMULP  | FMUL
    | FSUBP  | FSUB  		(* ST(1) := ST-ST(1); [pop] *)
    | FSUBRP | FSUBR		(* ST(1) := ST(1)-ST; [pop] *)
    | FDIVP  | FDIV		(* ST(1) := ST/ST(1); [pop] *)
    | FDIVRP | FDIVR 		(* ST(1) := ST(1)/ST; [pop] *)

  datatype funOp = FABS | FCHS 

 (* many of these instructions imply certain register usages *)
  datatype instruction =
      NOP
    | JMP of operand * Label.label list
    | JCC of {cond:cond, opnd:operand}
    | CALL of operand * C.cellset * C.cellset
    | RET

   (* integer *)
    | MOVE of {mvOp:move, src:operand, dst:operand}
    | LEA of {r32: int, addr: operand}
    | CMP of {lsrc: operand, rsrc: operand}
    | BINARY of {binOp:binaryOp, src:operand, dst:operand}
    | MULTDIV of {multDivOp:multDivOp, src:operand}
    | MUL3 of {dst:int, src1:operand, src2: Int32.int option}
    | UNARY of {unOp:unaryOp, opnd:operand}
    | PUSH of operand
    | POP of operand
    | CDQ
    | INTO

   (* parallel copies *)
    | COPY of {dst:int list, src:int list, tmp:operand option}
    | FCOPY of {dst:int list, src:int list, tmp:operand option}

   (* floating *)
    | FBINARY of {binOp:fbinOp, src:operand, dst:operand}
    | FUNARY of funOp
    | FUCOMPP
    | FXCH
    | FSTP of operand
    | FLD of operand
    | FILD of operand
    | FNSTSW
   (* misc *)
    | SAHF
end

