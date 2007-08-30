(* Copyright (c) 1998 YALE FLINT PROJECT *)
(* ltyutil.sig *)

(*
 * This file contains the common utility functions used to manipulate
 * kinds, tycs, and ltys.
 *
 * The rule of thumb about what should be included in this file rather
 * than the ltyextern.sml: well, all primitive lambda tkinds, tycs and 
 * ltys should be here, all common utility functions on tkinds, tycs, 
 * and ltys should be here. Functions that are of specific use should 
 * go to the ltyextern.sml. Still, the module LtyExtern will include 
 * all functions defined here, so all clients should use functions via
 * the LtyExtern structure. 
 *)

signature LTYUTIL = 
sig

(*
 * The abstract definitions of tkind, tyc, and lty are in Lty (lty.sig/sml).
 * In general, the clients of the lambda types should never need to
 * understand what is going on inside Lty (e.g. hash-consing of types).
 *)

(** primitives and utility functions for Lty.fflags and rflags *)
val ffc_plambda: Lty.fflag
val ffc_rrflint: Lty.fflag
val ffc_fspec  : Lty.fflag * (bool * bool) -> Lty.fflag
val ffd_fspec  : Lty.fflag -> bool * bool

(** primitive lambda tycs *)
val tcc_int    : Lty.tyc
val tcc_int32  : Lty.tyc
val tcc_real   : Lty.tyc
val tcc_string : Lty.tyc
val tcc_exn    : Lty.tyc
val tcc_void   : Lty.tyc
val tcc_unit   : Lty.tyc
val tcc_bool   : Lty.tyc
val tcc_list   : Lty.tyc

val tcc_tv     : int -> Lty.tyc
val tcc_ref    : Lty.tyc -> Lty.tyc
val tcc_array  : Lty.tyc -> Lty.tyc
val tcc_vector : Lty.tyc -> Lty.tyc
val tcc_etag   : Lty.tyc -> Lty.tyc

(** primitive lambda ltys *)
val ltc_int    : Lty.lty
val ltc_int32  : Lty.lty
val ltc_real   : Lty.lty
val ltc_string : Lty.lty
val ltc_exn    : Lty.lty
val ltc_void   : Lty.lty
val ltc_unit   : Lty.lty
val ltc_bool   : Lty.lty

val ltc_tv     : int -> Lty.lty
val ltc_ref    : Lty.lty -> Lty.lty
val ltc_array  : Lty.lty -> Lty.lty
val ltc_vector : Lty.lty -> Lty.lty
val ltc_etag   : Lty.lty -> Lty.lty

val ltc_top    : Lty.lty    (* used in a dirty hack in prim.sml *)

(** adjusting an lty or tyc from one depth to another *)
val lt_adj     : Lty.lty * DebIndex.depth * DebIndex.depth -> Lty.lty
val tc_adj     : Lty.tyc * DebIndex.depth * DebIndex.depth -> Lty.tyc

val lt_adj_k   : Lty.lty * DebIndex.depth * DebIndex.depth * int -> Lty.lty  
val tc_adj_k   : Lty.tyc * DebIndex.depth * DebIndex.depth * int -> Lty.tyc  

(** the ltyEnv maps from lvar to its lty; notice lty is depth-dependent *)
type ltyEnv 
exception ltUnbound
val initLtyEnv : ltyEnv
val ltLookup : ltyEnv * LambdaVar.lvar * DebIndex.depth -> Lty.lty
val ltInsert : ltyEnv * LambdaVar.lvar * Lty.lty * DebIndex.depth -> ltyEnv

end (* signature LTYUTIL *)
