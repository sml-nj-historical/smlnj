(* x86Instr.sml -- 32bit, x86 instruction set.
 *
 * COPYRIGHT (c) 1997 Bell Laboratories.
 *
 * $Log: X86Instr.sml,v $
 * Revision 1.2  1998/08/27 14:12:15  george
 *   used sethi-ullman number for floating point stack
 *
 * Revision 1.1.1.1  1998/07/22 18:10:32  george
 *   X86.1
 *
 *)
functor X86Instr(structure Const : CONSTANT
		 structure Region : REGION) : X86INSTR = struct
  structure C = X86Cells
  structure Constant = Const
  structure Region = Region

  type csets = int list * int list * int list

  datatype operand =
     Immed      of Int32.int
   | ImmedLabel of LabelExp.labexp
   | Const      of Constant.const
   | Direct     of int
   | FDirect    of int
   | Relative   of int
   | LabelEA	of LabelExp.labexp
   | Displace   of {base:int, disp:operand}
   | Indexed    of {base:int option, index:int, scale:int, disp:operand}

  type ea = operand
  
  datatype binaryOp = ADD | SUB  | AND | OR | XOR | SHL | SAR | SHR

  datatype multDivOp = UMUL |  IDIV | UDIV

  datatype unaryOp = DEC | INC | NEG | NOT

  datatype move = MOVL | MOVZX | MOVB

  datatype cond = 
      EQ | NE  | LT | LE  | GT | GE  | B | BE  | A | AE | C | NC
    | P | NP | O | NO

  datatype fbinOp = 
      FADDP | FADD | FSUBP | FSUB | FMULP | FMUL | FDIVP | FDIV
    | FDIVRP | FDIVR | FSUBRP | FSUBR

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
    | MUL3 of {src1:operand, src2:Int32.int option, dst:int}
    | UNARY of {unOp:unaryOp, opnd:operand}
    | PUSH of operand
    | POP of operand
    | CDQ
    | INTO

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


