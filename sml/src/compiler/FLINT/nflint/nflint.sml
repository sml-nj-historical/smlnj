(* nflint.sml *)

structure NFlint = struct

local
    structure LV = LambdaVar
(*  
    structure PO = PrimOp
    structure PT = PrimTyc 
*)
in

type tvar = LV.lvar   
type lvar = LV.lvar

datatype nty
  = NT_VAR of tvar                            (* named nty variable *)
  | NT_INT
  | NT_SINT of int                            (* singleton integer type *)

  | NT_ARRAY of nty                           (* array constructor *)
  | NT_REF of nty                             (* ref constructor *)  

  | NT_FIX of tvar * int * nty list           (* recursive sum type *) 
  | NT_TUPLE of nty list                      (* std record type *)
  | NT_EXIST of tvar * nty                    (* existential type *)

  | NT_CODE of tvar list * nty list * nty list (* code type *)

datatype accesspath
  = OFFp of int
  | SELp of int * accesspath

datatype fkind
  = KNOWN
  | CONT
  | ESCAPE of nty list option

datatype cmpop 
  = IEQ | INEQ | ILEQ | IGEQ | ILT | IGT 

datatype primop 
  = MUL | DIV | MOD | SUB | ADD 
  | MKARRAY | SUBSCRIPT | UPDATE | REF | DEREF | ASSIGN

datatype value
  = VAR   of lvar
  | LABEL of lvar
  | INT   of int                            
  | SINT  of int                            

datatype lexp
  = RET of value list         
  | LET of lvar list * lexp * lexp

  | FIX of fundec list * lexp 
  | APP of value * nty list * value list  

  | PTAPP  of value * nty list * lvar * lexp   (* partial application of a function to a type argument,
                                                  c.f. TAL paper Sec 5 *) 

  | PACK   of nty * value * nty * lvar * lexp
  | UNPACK of tvar * lvar * value * lexp

  | SWITCH of value * lexp list * (lvar * lexp) list 
  | CON    of value * nty * lvar * lexp

  | RECORD of (value * accesspath) list * lvar * lexp
  | SELECT of value * int * lvar * lexp

  | BRANCH of cmpop * value list * lexp * lexp
  | PRIMOP of primop * value list * lvar * lexp

withtype fundec = fkind * lvar * tvar list * (lvar * nty) list * lexp

type prog = fundec

end (* toplevel local *)
end (* structure NFlint *)
