(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* flint.sig *)

signature FLINT = 
sig

type tkind = LtyDef.tkind
type tyc = LtyDef.tyc
type lty = LtyDef.lty
type tvar = LtyDef.tvar 
type lvar = LambdaVar.lvar

(** classifying various kinds of functions *)
type fkind = 
  {isrec : lty list option, 
   raw   : LtyDef.rawflag,
   isfct : bool}
(*
 * rec --- NONE means non-recursive
 *	      SOME t means that t is the function type.
 * raw --- true means that argument/result flattening has been done
 *
 * In the future, we want to cover the following and more:
 *
 *  FK_KREC            known recursive functions 
 *  FK_KNOWN           general known functions 
 *  FK_ESCAPE          general escaping functions 
 *  FK_KTAIL           tail-recursive kernal 
 *  FK_NOINL           no in-line expansions are allowed inslide 
 *  FK_HANDLER         exception handler 
 *)

(** classifying various kinds of records *)
datatype rkind
  = RK_VECTOR of tyc  (* vector: all elements have same type *)
  | RK_RECORD         (* tuple: all elements are monomorphic *)
  | RK_STRUCT         (* module: elements may be polymorphic *)

(*
 * dcon records the name of the constructor (for debugging), the 
 * corresponding conrep, and the lambda type lty; the value carrying 
 * data constructors must have the arrow type. 
 *)
type dcon = Symbol.symbol * Access.conrep * lty

(*
 * con: used to specify all possible switching statements. Efficient switch
 * generation can be applied to DATAcon and INTcon. Otherwise, the switch is 
 * just a short-hand of the binary branch trees. Some of these instances 
 * such as REALcon probably should go away.
 *)
datatype con 
  = DATAcon of dcon * tyc list * lvar list
  | INTcon of int                          (* should use InfInf.int *)
  | INT32con of Int32.int 
  | WORDcon of word 
  | WORD32con of Word32.word 
  | REALcon of string 
  | STRINGcon of string 
  | VLENcon of int                  (* ??? require checks on vec select *) 

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
  | CON of dcon * tyc list * value list * lvar * lexp  

  | RECORD of rkind * value list * lvar * lexp
  | SELECT of value * int * lvar * lexp          (* add rkind ? *)

  | RAISE of value * lty list
  | HANDLE of lexp * value
  | ETAG of tyc * value * lvar * lexp            

  | PRIMOP of primop * value list * lvar * lexp
  | GENOP of dict * primop * value list * lvar * lexp

  | WRAP of tyc * value * lvar * lexp
  | UNWRAP of tyc * value * lvar * lexp

(** the return type is not required for non-recursive functions *)
withtype fundec = fkind * lvar * (lvar * lty) list * lexp

(** definitions of type functions, result type not required *)
and tfundec = lvar * (tvar * tkind) list * lexp

(** used by GENOP only, should go away soon *)
and dict = {default: lvar, table: (tyc list * lvar) list}

and primop = PrimOp.primop * lty * tyc list

type prog = fundec (* was "lvar * lty * lexp" *)

end (* signature FLINT *)
