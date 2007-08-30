(* Copyright (c) 1998 YALE FLINT PROJECT *)
(* ltyextern.sig *)

(*
 * This interface hides the implementation details of FLINT tkind, tyc, and 
 * lty defined inside Lty. For each entity, we provide a series of 
 * constructor funtions, deconstructor functions, predicate functions,
 * and other utility functions. We divide these functions into three files:
 * LtyStructure contains the set of abstract constructor, deconstructor, and
 * predicate functions for tkind, tyc, and lty; LtyBasic includes all 
 * functions in LtyStructure plus all commonly used primitive tycs and ltys, and
 * utility functions; finally, the current LtyExtern structure includes all
 * functions in LtyBasic plus a set of rather specialized utility functions.
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
 * PrimTyc, Symbol, and Lty, and LtyUtil.
 *)

signature LTYEXTERN =
sig

(*
 * We left the definitions of tkind, tyc, and lty in a separate file, i.e.,
 * ltydef.sig and ltydef.sml. The idea is that those two files should change
 * very rarely while the interface for LtyExtern may change often. The client
 * should refer to Lty for the use of type names, and to LtyExtern for the
 * use of utility functions.
 *)
include LTYSTRUCTURE   (* ltydef.sig (=> ltystructure.sig) *)
include LTYUTIL        (* ltybasic.sig (=> ltyutil.sig) *)
include LTYMISC        (* ltymisc.sig *)

type tvar = Lty.tvar
type tkind = Lty.tkind
type tyc = Lty.tyc
type lty = Lty.lty
type tkindEnv = Lty.tkindEnv

(** tkind constructors *)
val tkc_mono   : tkind
val tkc_box    : tkind
val tkc_seq    : tkind list -> tkind
val tkc_fun    : tkind list * tkind -> tkind

(** tkind deconstructors *)
val tkd_mono   : tkind -> unit
val tkd_box    : tkind -> unit
val tkd_seq    : tkind -> tkind list
val tkd_fun    : tkind -> tkind list * tkind

(** tkind predicates *)
val tkp_mono   : tkind -> bool
val tkp_box    : tkind -> bool
val tkp_seq    : tkind -> bool
val tkp_fun    : tkind -> bool

(** tkind one-arm switch *)
val tkw_mono   : tkind * (unit -> 'a) * (tkind -> 'a) -> 'a
val tkw_box    : tkind * (unit -> 'a) * (tkind -> 'a) -> 'a
val tkw_seq    : tkind * (tkind list -> 'a) * (tkind -> 'a) -> 'a
val tkw_fun    : tkind * (tkind list * tkind -> 'a) * (tkind -> 'a) -> 'a

val tkc_int : int -> tkind
val tkc_arg : int -> tkind list

val initTkEnv : tkindEnv
val tkInsert  : tkindEnv * tkind list -> tkindEnv

val mkTvar : unit -> tvar                     (* used: reps/coerce.sml *)


(* from LtyNorm *)
exception TeUnbound
exception TCENV

val tk_eqv   : Lty.tkind * Lty.tkind -> bool (* opt/specialize.sml *)
val tc_eqv   : Lty.tyc * Lty.tyc -> bool (* used in plambda/flintnm.sml *)
val lt_eqv   : Lty.lty * Lty.lty -> bool
val ff_eqv   : Lty.fflag * Lty.fflag -> bool (* opt/specialize.sml *)

val lt_autoflat : Lty.lty -> bool * Lty.lty list * bool

val tc_unknown : Lty.tyc -> bool

end (* signature LTYEXTERN *)
