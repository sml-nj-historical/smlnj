(* plambda.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PLAMBDA = sig

type tkind = PLambdaType.tkind
type tyc = PLambdaType.tyc
type lty = PLambdaType.lty
type lvar = LambdaVar.lvar

type dataconstr = Symbol.symbol * Access.conrep * lty

datatype con
  = DATAcon of dataconstr * tyc list * lvar
  | INTcon of int
  | INT32con of Int32.int
  | INTINFcon of IntInf.int		(* only used with in matchcomp *)
  | WORDcon of word
  | WORD32con of Word32.word
  | STRINGcon of string
  | VLENcon of int

datatype lexp
  = VAR of lvar
  | INT of int
  | INT32 of Int32.int
  | WORD of word
  | WORD32 of Word32.word
  | REAL of RealLit.t
  | STRING of string
  | PRIM of Primop.primop * lty * tyc list
  | GENOP of dict * Primop.primop * lty * tyc list

  | FN of lvar * lty * lexp
  | FIX of lvar list * lty list * lexp list * lexp
  | APP of lexp * lexp
  | LET of lvar * lexp * lexp

  | TFN of tkind list * lexp
  | TAPP of lexp * tyc list

  | RAISE of lexp * lty
  | HANDLE of lexp * lexp
  | ETAG of lexp * lty

  | CON of dataconstr * tyc list * lexp
  | SWITCH of lexp * Access.consig * (con * lexp) list * lexp option

  | VECTOR of lexp list * tyc
  | RECORD of lexp list
  | SRECORD of lexp list
  | SELECT of int * lexp

  | PACK of lty * tyc list * tyc list * lexp
  | WRAP of tyc * bool * lexp
  | UNWRAP of tyc * bool * lexp

withtype dict = {default: lexp, table: (tyc list * lexp) list}

end (* signature PLAMBDA *)

