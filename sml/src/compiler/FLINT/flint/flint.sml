(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* flint.sml *)

structure FLINT : FLINT = 
struct

local structure A  = Access   (* should go away soon *)
      structure LD = LtyDef
      structure LV = LambdaVar
      structure PO = PrimOp
      structure S  = Symbol
in 

type tkind = LD.tkind
type tyc = LD.tyc
type lty = LD.lty
type tvar = LV.lvar   
type lvar = LV.lvar

(** classifying various kinds of functions *)
type fkind = 
  {isrec : lty list option, 
   raw   : LD.rawflag, 
   isfct : bool}

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
type dcon = S.symbol * A.conrep * lty

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

  | SWITCH of value * A.consig * (con * lexp) list * lexp option
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
(*
 * | PACK of lty * tyc list * tyc list * value * lvar * lexp
 * | BRANCH of primop * value list * lexp * lexp   (* comparison *)
 * | ARITH of primop  * value list * lvar * lexp   (* arithmetic *)
 * | SETTER of primop * value list * lexp          (* mutator *)
 * | LOOKER of primop * value list * lvar * lexp   (* selector *)
 * | PURE of primop * value list * lvar * lexp     (* constructor? *)
 *)

withtype fundec = fkind * lvar * (lvar * lty) list * lexp
and tfundec = lvar * (tvar * tkind) list * lexp
and primop = PO.primop * lty * tyc list
and dict = {default: lvar, table: (tyc list * lvar) list}

type prog = fundec                       (* was "lvar * lty * lexp" *)

(* 
 * TODO: 0. what can we expect from the use of "tvar" ?
 *       1. organize the set of valid primops and primtycs
 *       2. organize the set of standard constants and constructors
 *       3. what is the std way of adding pre-defined GENOP ?
 *       4. how to clean up pack, wrap, and unwrap ?
 *       5. how to standardize the fkind and rkind ?
 *       6. how to handle the wrappers in CON and DECON (SWITCH) ?
 *       7. how to avoid the use of conrep ? what about exceptions ?
 *)

end (* local *)
end (* structure FLINT *)
