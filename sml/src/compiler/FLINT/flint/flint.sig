(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* flint.sig *)

signature FLINT = 
sig

type tkind = LtyDef.tkind
type tyc = LtyDef.tyc
type lty = LtyDef.lty

type tvar = LtyDef.tvar 
type lvar = LambdaVar.lvar

type fflag = LtyDef.fflag
type rflag = LtyDef.rflag

(** classifying various kinds of functions *)
datatype fkind 
  = FK_FCT
  | FK_FUN of 
      {isrec : lty list option,  (* is this function recursive ? *)
       fixed : fflag,            (* is calling convention fixed ? *)
       known : bool,             (* is this function known, default: false *)
       inline: bool}             (* should this be inlined, default: false *)

(** classifying various kinds of records *)
datatype rkind
  = RK_VECTOR of tyc           (* vector: all elements have same type *)
  | RK_STRUCT                  (* module: elements may be polymorphic *)
  | RK_TUPLE of rflag          (* tuple: all fields are monomorphic *)

(*
 * dcon records the name of the constructor (for debugging), the 
 * corresponding conrep, and the flint type lty (which must be an
 * arrow type). The use of conrep will go away soon.
 *)
type dcon = Symbol.symbol * Access.conrep * lty

(*
 * con: used to specify all possible switching statements. Efficient switch
 * generation can be applied to DATAcon and INTcon. Otherwise, the switch is 
 * just a short-hand of the binary branch trees. Some of these instances 
 * such as REALcon and VLENcon will go away soon.
 *)
datatype con 
  = DATAcon of dcon * tyc list * lvar 
  | INTcon of int                          (* should use InfInf.int *)
  | INT32con of Int32.int 
  | WORDcon of word 
  | WORD32con of Word32.word 
  | REALcon of string 
  | STRINGcon of string 
  | VLENcon of int 

(** simple values, including variables and static constants. *)
datatype value
  = VAR of lvar
  | INT of int                            (* should use InfInf.int *)
  | INT32 of Int32.int
  | WORD of word
  | WORD32 of Word32.word
  | REAL of string
  | STRING of string

(** the definitions of the lambda expressions *)
datatype lexp
  = RET of value list         
  | LET of lvar list * lexp * lexp

  | FIX of fundec list * lexp 
  | APP of value * value list  

  | TFN of tfundec * lexp
  | TAPP of value * tyc list 

  | SWITCH of value * Access.consig * (con * lexp) list * lexp option
  | CON of dcon * tyc list * value * lvar * lexp  

  | RECORD of rkind * value list * lvar * lexp
  | SELECT of value * int * lvar * lexp          (* add rkind ? *)

  | RAISE of value * lty list                    
  | HANDLE of lexp * value

  | BRANCH of primop * value list * lexp * lexp
  | PRIMOP of primop * value list * lvar * lexp

withtype fundec = fkind * lvar * (lvar * lty) list * lexp
and tfundec = lvar * (tvar * tkind) list * lexp  
and dict = {default: lvar, table: (tyc list * lvar) list} 
and primop = dict option * PrimOp.primop * lty * tyc list 
        (* Invariant: primop's lty is always fully closed *)

type prog = fundec  (* was "lvar * lty * lexp" *)

end (* signature FLINT *)
