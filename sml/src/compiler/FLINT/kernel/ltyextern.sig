(* Copyright (c) 1998 YALE FLINT PROJECT *)
(* ltyextern.sig *)

(*
 * This interface hides the implementation details of FLINT tkind, tyc, and 
 * lty defined inside LtyKernel. For each entity, we provide a series of 
 * constructor funtions, deconstructor functions, predicate functions,
 * and other utility functions. We divide these functions into three files:
 * LtyDef contains the set of abstract constructor, deconstructor, and
 * predicate functions for tkind, tyc, and lty; LtyBasic includes all 
 * functions in LtyDef plus all commonly used primitive tycs and ltys, and
 * utility functions; finally, the current LtyExtern structure includes all
 * functions in LtyBasic plus a set of rather specialized utility functions.
 *
 * We design this hierarchy in a way so that LtyDef as a stable interface, 
 * so one can refer to types such as tkind, tyc, lty as LtyDef.tkind, etc;
 * LtyBasic is a medium-stable interface, only commonly used functions 
 * should be left here; LtyExtern is a least-stable interface, any new 
 * utility function that manipulates types should go here.
 *
 * The conventions are (1) types should be referenced as "LtyDef.foo"
 * (2) functions should all be accessed as "LtyExtern.foo". The client
 * in general should never need to access LtyKernel.
 *
 * This interface should only refer to structures such as DebIndex, LtyKernel, 
 * PrimTyc, Symbol, and LtyBasic (indirectly LtyDef).
 *)

signature LTYEXTERN =
sig

(*
 * We left the definitions of tkind, tyc, and lty in a separate file, i.e.,
 * ltydef.sig and ltydef.sml. The idea is that those two files should change
 * very rarely while the interface for LtyExtern may change often. The client
 * should refer to LtyDef for the use of type names, and to LtyExtern for the
 * use of utility functions.
 *)
include LTYBASIC        (* see ltydef.sig and ltybasic.sig for details *)


(** instantiating a polymorphic type or an higher-order constructor *)
val lt_inst     : lty * tyc list -> lty list
val lt_pinst    : lty * tyc list -> lty
val lt_inst_st  : lty * tyc list -> lty list   (* strict apply *)
val lt_pinst_st : lty * tyc list -> lty        (* strict apply *)

exception TkTycChk
exception LtyAppChk
val lt_inst_chk : lty * tyc list * tkindEnv -> lty list

(* special adjustment functions used during type specializations *)
val lt_sp_adj : tkind list * lty * tyc list * int * int -> lty
val tc_sp_adj : tkind list * tyc * tyc list * int * int -> tyc
val lt_sp_sink: tkind list * lty * depth * depth -> lty
val tc_sp_sink: tkind list * tyc * depth * depth -> tyc

(** utility functions used in CPS only, should go away soon ! *)
val lt_iscont   : lty -> bool
val ltw_iscont  : lty * (lty list -> 'a) * (tyc list -> 'a) * (lty -> 'a) -> 'a

(** other utility functions --- requires clean up!*)
val lt_select : lty * int -> lty
val lt_swap : lty -> lty

(** functions that manipulate the FLINT function and record types *)
val ltc_fkfun   : FLINT.fkind * lty list * lty list -> lty
val ltd_fkfun   : lty -> lty list * lty list (* fkind omitted *)

val ltc_rkind   : FLINT.rkind * lty list -> lty
val ltd_rkind   : lty * int -> lty

val ltc_arw     : lty * lty -> lty
val ltc_fun     : lty * lty -> lty

val lt_arrow    : lty -> lty * lty
val lt_arrowN   : lty -> lty list * lty list

val tc_upd_prim : tyc -> PrimOp.primop

end (* signature LTYEXTERN *)

