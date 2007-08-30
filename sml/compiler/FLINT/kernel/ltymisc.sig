(* Copyright (c) 1998 YALE FLINT PROJECT *)
(* ltyextern.sig *)

(*
 * This interface hides the implementation details of FLINT tkind, tyc, and 
 * lty defined inside Lty. For each entity, we provide a series of 
 * constructor funtions, deconstructor functions, predicate functions,
 * and other utility functions. We divide these functions into three files:
 * LtyStructure contains the set of abstract constructor, deconstructor, and
 * predicate functions for tkind, tyc, and lty; LtyUtil includes all 
 * all commonly used
 * utility functions; finally, the current LtyExtern structure includes all
 * functions in LtyUtil plus a set of rather specialized utility functions
 * defined in LtyMisc (formerly part of LtyExtern).
 *
 * We design this hierarchy in a way so that Lty as a stable interface, 
 * so one can refer to types such as tkind, tyc, lty as Lty.tkind, etc;
 * LtyUtil is a medium-stable interface, only commonly used functions 
 * should be left here; LtyExtern is a least-stable interface, any new 
 * utility function that manipulates types should go here.
 *
 * The conventions are (1) types should be referenced as "Lty.foo"
 * (2) functions should all be accessed as "LtyExtern.foo". The client
 * in general should never need to access LtyNorm.
 *
 * This interface should only refer to structures such as DebIndex, Lty, 
 * PrimTyc, Symbol.
 *)

signature LTYMISC =
sig

(*
 * We left the definitions of tkind, tyc, and lty in a separate file, i.e.,
 * ltydef.sig and ltydef.sml. The idea is that those two files should change
 * very rarely while the interface for LtyExtern may change often. The client
 * should refer to Lty for the use of type names, and to LtyExtern for the
 * use of utility functions.
 *)

(** instantiating a polymorphic type or an higher-order constructor *)
val lt_inst     : Lty.lty * Lty.tyc list -> Lty.lty list
val lt_pinst    : Lty.lty * Lty.tyc list -> Lty.lty

exception LtyAppChk
val lt_inst_chk_gen : unit -> Lty.lty * Lty.tyc list * Lty.tkindEnv -> Lty.lty list

(* substitution of named type variables *)
(*** CLEAN THIS UP ***)
val tc_nvar_elim_gen : unit -> (Lty.tvar * DebIndex.depth -> Lty.tyc option) 
                            -> DebIndex.depth -> Lty.tyc -> Lty.tyc
val lt_nvar_elim_gen : unit -> (Lty.tvar * DebIndex.depth -> Lty.tyc option) 
                            -> DebIndex.depth -> Lty.lty -> Lty.lty

(* !! BEWARE !!
 * The `subst' argument is assumed to be sorted with increasing tvars *)
val tc_nvar_subst_gen : unit -> (Lty.tvar * Lty.tyc) list -> Lty.tyc -> Lty.tyc
val lt_nvar_subst_gen : unit -> (Lty.tvar * Lty.tyc) list -> Lty.lty -> Lty.lty

val tc_nvar_cvt_gen : unit -> (Lty.tvar * int) list 
                           -> DebIndex.depth -> Lty.tyc -> Lty.tyc
val lt_nvar_cvt_gen : unit -> (Lty.tvar * int) list 
                           -> DebIndex.depth -> Lty.lty -> Lty.lty
(* The equivalent to ltc_poly for the nvar case *)
val lt_nvpoly : (Lty.tvar * Lty.tkind) list * Lty.lty list -> Lty.lty

(* special adjustment functions used during type specializations *)
val lt_sp_adj : Lty.tkind list * Lty.lty * Lty.tyc list * int * int -> Lty.lty
val tc_sp_adj : Lty.tkind list * Lty.tyc * Lty.tyc list * int * int -> Lty.tyc
val lt_sp_sink: Lty.tkind list * Lty.lty * DebIndex.depth * DebIndex.depth -> Lty.lty
val tc_sp_sink: Lty.tkind list * Lty.tyc * DebIndex.depth * DebIndex.depth -> Lty.tyc

(** utility functions used in CPS only, should go away soon ! *)
val lt_iscont   : Lty.lty -> bool
val ltw_iscont  : Lty.lty * (Lty.lty list -> 'a) * (Lty.tyc list -> 'a) * (Lty.lty -> 'a) -> 'a

(** other utility functions --- requires clean up!*)
val lt_select : Lty.lty * int -> Lty.lty
val lt_swap : Lty.lty -> Lty.lty

(** functions that manipulate the FLINT function and record types *)
val ltc_fkfun   : FLINT.fkind * Lty.lty list * Lty.lty list -> Lty.lty
val ltd_fkfun   : Lty.lty -> Lty.lty list * Lty.lty list (* fkind omitted *)

val ltc_rkind   : FLINT.rkind * Lty.lty list -> Lty.lty
val ltd_rkind   : Lty.lty * int -> Lty.lty

(** given a tyc, select the appropriate update primop *)
val tc_upd_prim : Lty.tyc -> PrimOp.primop

(** translating the tkind into the corresponding type *)
val tk_lty      : Lty.tkind -> Lty.lty

(** twrap type translation generator, used by Wrapping.wrapping *)
val twrap_gen   : bool -> ((Lty.tyc -> Lty.tyc) * (Lty.lty -> Lty.lty) *
                           (Lty.tyc -> Lty.tyc) * (Lty.lty -> Lty.lty) * (unit -> unit))

(** tnarrow type translation generator, used by Reify.reify *)
val tnarrow_gen : unit -> ((Lty.tyc -> Lty.tyc) * (Lty.lty -> Lty.lty) * (unit -> unit))

end (* signature LTYMISC *)
