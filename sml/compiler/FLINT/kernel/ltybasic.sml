(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltyutil.sml *)

(* general purpose utility functions on Lty types *)
(* was LtyBasic *)

structure LtyUtil : LTYUTIL = 
struct

local
  structure PT = PrimTyc
  structure DI = DebIndex
  structure LT = Lty
  structure LN = LtyNorm

  fun bug msg = ErrorMsg.impossible("LtyUtil: "^msg)
  val say = Control.Print.say

  open Lty LtyStructure

  (** common utility functions *)

  val ltc_env = LN.ltc_env

in

(** utility functions for constructing tkinds *)
fun tkc_arg n = 
  let fun h (n, r) = if n < 1 then r else h(n-1, tkc_mono::r)
   in h(n, [])
  end

val tkc_fn1 = tkc_fun(tkc_arg 1, tkc_mono)
val tkc_fn2 = tkc_fun(tkc_arg 2, tkc_mono)
val tkc_fn3 = tkc_fun(tkc_arg 3, tkc_mono)

fun tkc_int 0 = tkc_mono
  | tkc_int 1 = tkc_fn1
  | tkc_int 2 = tkc_fn2
  | tkc_int 3 = tkc_fn3
  | tkc_int i = tkc_fun(tkc_arg i, tkc_mono)

(** primitive fflags and rflags *)
val ffc_plambda = ffc_var (false, false)
val ffc_rrflint = ffc_var (true, true)

fun ffc_fspec (x as LT.FF_FIXED, (true,true)) = x
  | ffc_fspec (x as LT.FF_VAR _, nx) = ffc_var nx
  | ffc_fspec _ = bug "unexpected case in ffc_fspec"

fun ffd_fspec (LT.FF_FIXED) = (true,true)
  | ffd_fspec (LT.FF_VAR x) = x

(** utility functions for constructing tycs *)
val tcc_int    = tcc_prim PT.ptc_int31
val tcc_int32  = tcc_prim PT.ptc_int32
val tcc_real   = tcc_prim PT.ptc_real
val tcc_string = tcc_prim PT.ptc_string
val tcc_exn    = tcc_prim PT.ptc_exn
val tcc_void   = tcc_prim PT.ptc_void
val tcc_unit   = tcc_tuple []
val tcc_bool   = 
  let val tbool = tcc_sum [tcc_unit, tcc_unit]
      val tsig_bool = tcc_fn ([tkc_mono], tbool)
   in tcc_fix((1, #["bool"], tsig_bool, []), 0)
  end

val tcc_list   =  (* used only in LtyToString *)
  let val alpha = tcc_var (DI.innermost, 0)
      val tlist = tcc_var (DI.innersnd, 0)
      val alist = tcc_app (tlist, [alpha])
      val tcc_cons = tcc_tuple [alpha, alist]
      val tlist = tcc_fn([tkc_mono], tcc_sum [tcc_cons, tcc_unit])
                            (** the order here should be consistent with
                                that in basics/basictypes.sml **)
      val tsig_list = tcc_fn([tkc_int 1], tlist)
   in tcc_fix((1, #["list"], tsig_list, []), 0)
  end

fun tcc_tv i     = tcc_var(DI.innermost, i)
fun tcc_ref x    = tcc_app(tcc_prim PT.ptc_ref, [x])
fun tcc_array x  = tcc_app(tcc_prim PT.ptc_array, [x])
fun tcc_vector x = tcc_app(tcc_prim PT.ptc_vector, [x])
fun tcc_etag x   = tcc_app(tcc_prim PT.ptc_etag, [x])

(** primitive lambda ltys *)
val ltc_int    = ltc_tyc tcc_int
val ltc_int32  = ltc_tyc tcc_int32
val ltc_real   = ltc_tyc tcc_real
val ltc_string = ltc_tyc tcc_string
val ltc_exn    = ltc_tyc tcc_exn
val ltc_void   = ltc_tyc tcc_void
val ltc_unit   = ltc_tyc tcc_unit
val ltc_bool   = ltc_tyc tcc_bool

val ltc_tv     = ltc_tyc o tcc_tv
val ltc_ref    = 
    fn x => (ltc_tyc o tcc_ref o ltd_tyc) x
            handle DeconExn => bug "ltc_ref on Poly"
val ltc_array  =
    fn x => (ltc_tyc o tcc_array o ltd_tyc) x
            handle DeconExn => bug "ltc_array on Poly"
val ltc_vector = 
    fn x => (ltc_tyc o tcc_vector o ltd_tyc) x
            handle DeconExn => bug "ltc_vector on Poly"
val ltc_etag   =
    fn x => (ltc_tyc o tcc_etag o ltd_tyc) x
            handle DeconExn => bug "ltc_etag on Poly"

val ltc_top = ltc_ppoly([tkc_mono], ltc_tv 0)

(***************************************************************************
 *            DEPTH ADJUSTMENT FUNCTIONS                                   *
 ***************************************************************************)

(** adjusting an lty or tyc from one depth to another *)
fun lt_adj (lt, d, nd) = 
  if d = nd then lt 
  else LN.ltc_env(lt, 0, nd - d, LT.teEmpty)

fun tc_adj (tc, d, nd) = 
  if d = nd then tc 
  else LN.tcc_env(tc, 0, nd - d, LT.teEmpty) 
       (* handle LN.TCENV => bug "tc_adj" *)

(** The following functions are similiar to lt_adj and tc_adj;
    they adjust an lty (or tyc) from depth d+k to depth nd+k,
    assuming the last k levels are type abstractions. So lt_adj
    is really lt_adj_k with k set to 0. Both functions are currently
    called only in lcontract.sml. *)
local
fun mkTycEnv (i, k, dd, te) = 
  if i >= k then te 
  else mkTycEnv(i+1, k, dd, LT.teCons(LT.Lamb(dd+i,[]),te))
  (* dbm: no ks available *)

in 
fun lt_adj_k (lt, d, nd, k) = 
  if d = nd then lt 
  else LN.ltc_env(lt, k, nd-d+k, mkTycEnv(0, k, nd-d, LT.teEmpty))

fun tc_adj_k (tc, d, nd, k) = 
  if d = nd then tc 
  else LN.tcc_env(tc, k, nd-d+k, mkTycEnv(0, k, nd-d, LT.teEmpty))
       handle LN.TCENV => bug "tc_adj_k"

end (* lt_adj_k and tc_adj_k *)


(***************************************************************************
 *            UTILITY FUNCTIONS ON LTY ENVIRONMENT                         *
 ***************************************************************************)

(** utility values and functions on ltyEnv *)
type ltyEnv = (lty * DebIndex.depth) IntRedBlackMap.map

exception ltUnbound
val initLtyEnv : ltyEnv = IntRedBlackMap.empty

fun ltLookup (venv, lv, nd) = 
  (case IntRedBlackMap.find(venv, lv)
     of NONE  => 
	  (say "**** hmmm, I didn't find the variable ";
	   say (Int.toString lv); say "\n";
	   raise ltUnbound)
      | SOME (lt, d) => 
	  if d=nd then lt
	  else if d > nd then bug "unexpected depth info in ltLookup"
	       else LN.ltc_env(lt, 0, nd - d, LT.teEmpty)
  (*easc*))

fun ltInsert (venv, lv, lt, d) = IntRedBlackMap.insert(venv, lv, (lt, d))

end (* top-level local *)
end (* structure LtyUtil *)
