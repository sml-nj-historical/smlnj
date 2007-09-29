(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltyextern.sml *)

structure LtyExtern : LTYEXTERN = 
struct

  (* these three structures have disjoint signatures consisting
   * of only value and exception components *)
  open LtyStructure  (* working with type structures *)
  open LtyUtil       (* general purpose utilities *)
  open LtyMisc       (* special purpose utilities *)

  (* basic types imported from Lty *)

  type tvar = Lty.tvar
  type tkind = Lty.tkind
  type tyc = Lty.tyc
  type lty = Lty.lty
  type tkindEnv = Lty.tkindEnv

  (* values imported from Lty *)

  (** tkind constructors *)
  val tkc_mono = Lty.tkc_mono
  val tkc_box = Lty.tkc_box
  val tkc_seq = Lty.tkc_seq
  val tkc_fun = Lty.tkc_fun
  val tkc_int = Lty.tkc_int
  val tkc_arg = Lty.tkc_arg

  (** tkind deconstructors *)
  val tkd_mono = Lty.tkd_mono
  val tkd_box = Lty.tkd_box
  val tkd_seq = Lty.tkd_seq
  val tkd_fun = Lty.tkd_fun

  (** tkind predicates *)
  val tkp_mono = Lty.tkp_mono
  val tkp_box = Lty.tkp_box
  val tkp_seq = Lty.tkp_seq
  val tkp_fun = Lty.tkp_fun

  (** tkind one-arm switch *)
  val tkw_mono = Lty.tkw_mono
  val tkw_box = Lty.tkw_box
  val tkw_seq = Lty.tkw_seq
  val tkw_fun = Lty.tkw_fun

  (** tkind environments *)
  val initTkEnv = Lty.initTkEnv
  val tkInsert = Lty.tkInsert

  val mkTvar = Lty.mkTvar

  (* imported from LtyNorm *)

  exception TeUnbound = LtyNorm.TeUnbound (* used in plambda/chkplexp.sml *)
  exception TCENV = LtyNorm.TCENV

  val tk_eqv = LtyNorm.tk_eqv  (* opt/specialize.sml *)
  val tc_eqv = LtyNorm.tc_eqv  (* used in plambda/flintnm.sml *)
  val lt_eqv = LtyNorm.lt_eqv
  val ff_eqv = LtyNorm.ff_eqv  (* opt/specialize.sml *)

  val lt_autoflat = LtyNorm.lt_autoflat (* used in plambda/pflatten.sml *)

  val tc_unknown = LtyNorm.tc_unknown (* reps/wrappiing.sml *)

end (* structure LtyExtern *)
