(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltyextern.sml *)

structure LtyExtern : LTYEXTERN = 
struct

local structure PT = PrimTyc
      structure DI = DebIndex
      structure LK = LtyKernel
      structure PO = PrimOp     (* really should not refer to this *)
      structure FL = FLINT

      fun bug msg = ErrorMsg.impossible("LtyExtern: "^msg)
      val say = Control.Print.say

      (** common utility functions *)
      val tk_inj = LK.tk_inj
      val tk_out = LK.tk_out

      val tc_inj = LK.tc_inj
      val tc_out = LK.tc_out 
 
      val lt_inj = LK.lt_inj
      val lt_out = LK.lt_out 

      val tcc_env = LK.tcc_env
      val ltc_env = LK.ltc_env
      val tc_whnm = LK.tc_whnm
      val lt_whnm = LK.lt_whnm
      val tc_norm = LK.tc_norm
      val lt_norm = LK.lt_norm

in

open LtyBasic

val tc_depth = LK.tc_depth
val tcs_depth = LK.tcs_depth

(** instantiating a polymorphic type or an higher-order constructor *)
fun lt_inst (lt : lty, ts : tyc list) = 
  let val nt = lt_whnm lt
   in (case ((* lt_outX *) lt_out nt, ts)
        of (LK.LT_POLY(ks, b), ts) => 
             let val nenv = LK.tcInsert(LK.initTycEnv, (SOME ts, 0))
              in map (fn x => ltc_env(x, 1, 0, nenv)) b
             end
         | (_, []) => [nt]   (* this requires further clarifications !!! *)
         | _ => bug "incorrect lty instantiation in lt_inst")
  end 

fun lt_pinst (lt : lty, ts : tyc list) = 
  (case lt_inst (lt, ts) of [y] => y | _ => bug "unexpected lt_pinst")

val lt_inst_st = (map lt_norm) o lt_inst   (* strict instantiation *)
val lt_pinst_st = lt_norm o lt_pinst   (* strict instantiation *)

exception TkTycChk
exception LtyAppChk

fun tkSel (tk, i) = 
  (case (tk_out tk)
    of (LK.TK_SEQ ks) => (List.nth(ks, i) handle _ => raise TkTycChk)
     | _ => raise TkTycChk)

fun tks_eqv (ks1, ks2) = tk_eqv(tkc_seq ks1, tkc_seq ks2)

fun tkApp (tk, tks) = 
  (case (tk_out tk)
    of LK.TK_FUN(a, b) => if tks_eqv(a, tks) then b else raise TkTycChk
     | _ => raise TkTycChk)

(* Warning: the following tkTyc function has not considered the
 * occurence of .TK_BOX, in other words, if there is TK_BOX present,
 * then the tk_tyc checker will produce wrong results. (ZHONG)
 *)
fun tk_tyc (t, kenv) = 
  let fun g x = 
        (case tc_out x
          of (LK.TC_VAR (i, j)) => tkLookup(kenv, i, j)
           | (LK.TC_NVAR _) => bug "TC_NVAR not supported yet in tk_tyc"
           | (LK.TC_PRIM pt) => tkc_int (PrimTyc.pt_arity pt)
           | (LK.TC_FN(ks, tc)) => 
               tkc_fun(ks, tk_tyc(tc, tkInsert(kenv, ks)))
           | (LK.TC_APP (tc, tcs)) => tkApp(g tc, map g tcs)
           | (LK.TC_SEQ tcs) => tkc_seq (map g tcs)
           | (LK.TC_PROJ(tc, i)) => tkSel(g tc, i)
           | (LK.TC_SUM tcs) => 
               let val _ = map (fn x => tk_eqv(g x, tkc_mono)) tcs
                in tkc_mono
               end
           | (LK.TC_FIX ((n, tc, ts), i)) =>
               let val k = g tc
                   val nk = case ts of [] => k 
                                     | _ => tkApp(k, map g ts)
                in (case (tk_out nk)
                     of LK.TK_FUN(a, b) => 
                          let val arg = case a of [x] => x
                                                | _ => tkc_seq a
                           in if tk_eqv(arg, b) then 
                                (if n = 1 then b else tkSel(arg, i))
                              else raise TkTycChk
                          end
                      | _ => raise TkTycChk)
               end
           | (LK.TC_ABS tc) => (tk_eqv(g tc, tkc_mono); tkc_mono)
           | (LK.TC_BOX tc) => (tk_eqv(g tc, tkc_mono); tkc_mono)
           | (LK.TC_TUPLE (_,tcs)) => 
               let val _ = map (fn x => tk_eqv(g x, tkc_mono)) tcs
                in tkc_mono
               end
           | (LK.TC_ARROW (_, ts1, ts2)) =>
               let val _ = map (fn x => tk_eqv(g x, tkc_mono)) ts1
                   val _ = map (fn x => tk_eqv(g x, tkc_mono)) ts2
                in tkc_mono
               end
           | _ => bug "unexpected TC_ENV or TC_CONT in tk_tyc")
   in g t 
  end

and tk_chk kenv (k, tc) = 
  if tk_eqv(k, tk_tyc(tc, kenv)) then () else raise TkTycChk

fun lt_inst_chk (lt : lty, ts : tyc list, kenv : tkindEnv) = 
  let val nt = lt_whnm lt
   in (case ((* lt_outX *) lt_out nt, ts)
        of (LK.LT_POLY(ks, b), ts) => 
             let val _ = ListPair.app (tk_chk kenv) (ks, ts)
                 fun h x = ltc_env(x, 1, 0, tcInsert(initTycEnv, (SOME ts, 0)))
              in map h b
             end
         | (_, []) => [nt]    (* ? problematic *)
         | _ => raise LtyAppChk)
  end

(** a special lty application --- used inside the translate/specialize.sml *)
fun lt_sp_adj(ks, lt, ts, dist, bnl) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then ltc_env(lt, ol, nl, tenv)
        else if abslevel > 0 then 
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltAdjSt"

      val btenv = tcInsert(initTycEnv, (SOME ts, 0))
      val nt = h(dist, 1, bnl, btenv)
   in nt (* was lt_norm nt *)
  end

(** a special tyc application --- used inside the translate/specialize.sml *)
fun tc_sp_adj(ks, tc, ts, dist, bnl) =
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then tcc_env(tc, ol, nl, tenv)
        else if abslevel > 0 then 
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in tcAdjSt"

      val btenv = tcInsert(initTycEnv, (SOME ts, 0))
      val nt = h(dist, 1, bnl, btenv)
   in nt (* was tc_norm nt *)
  end

(** sinking the lty one-level down --- used inside the specialize.sml *)
fun lt_sp_sink (ks, lt, d, nd) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then ltc_env(lt, ol, nl, tenv)
        else if abslevel > 0 then
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltSinkSt"
      val nt = h(nd-d, 0, 1, initTycEnv)
   in nt (* was lt_norm nt *)
  end

(** sinking the tyc one-level down --- used inside the specialize.sml *)
fun tc_sp_sink (ks, tc, d, nd) = 
  let fun h(abslevel, ol, nl, tenv) =
        if abslevel = 0 then tcc_env(tc, ol, nl, tenv)
        else if abslevel > 0 then
               h(abslevel-1, ol+1, nl+1, tcInsert(tenv, (NONE, nl)))
             else bug "unexpected cases in ltSinkSt"
      val nt = h(nd-d, 0, 1, initTycEnv)
   in nt (* was tc_norm nt *)
  end

(** utility functions used in CPS *)
fun lt_iscont lt = 
      (case lt_out lt
        of LK.LT_CONT _ => true
         | LK.LT_TYC tc => 
             (case tc_out tc of LK.TC_CONT _ => true | _ => false)
         | _ => false)

fun ltw_iscont (lt, f, g, h) = 
      (case lt_out lt
        of LK.LT_CONT t => f t
         | LK.LT_TYC tc => 
             (case tc_out tc of LK.TC_CONT x => g x | _ => h lt)
         | _ => h lt)


fun tc_bug tc s = bug (s ^ "\n\n" ^ (tc_print tc) ^ "\n\n")
fun lt_bug lt s = bug (s ^ "\n\n" ^ (lt_print lt) ^ "\n\n")

(** other misc utility functions *)
fun tc_select(tc, i) = 
  (case tc_out tc
    of LK.TC_TUPLE (_,zs) =>
         ((List.nth(zs, i)) handle _ => bug "wrong TC_TUPLE in tc_select")
     | _ => tc_bug tc "wrong TCs in tc_select")

fun lt_select(t, i) = 
  (case lt_out t
    of LK.LT_STR ts => 
         ((List.nth(ts, i)) handle _ => bug "incorrect LT_STR in lt_select")
     | LK.LT_PST ts => 
         let fun h [] = bug "incorrect LT_PST in lt_select"
               | h ((j,a)::r) = if i=j then a else h r
          in h ts
         end
     | LK.LT_TYC tc => ltc_tyc(tc_select(tc, i))
     | _ => bug "incorrect lambda types in lt_select")

fun tc_swap t = 
  (case (tc_out t)
    of LK.TC_ARROW ((r1,r2), [s1], [s2]) => tcc_arrow((r2,r1), [s2], [s1])
     | _ => bug "unexpected tycs in tc_swap")

fun lt_swap t = 
  (case (lt_out t)
    of (LK.LT_POLY (ks, [x])) => ltc_poly(ks, [lt_swap x])
     | (LK.LT_TYC x) => ltc_tyc(tc_swap x)
     | _ => bug "unexpected type in lt_swap")

(** functions that manipulate the FLINT function and record types *)
fun ltc_fkfun (FL.FK_FCT, atys, rtys) = 
      ltc_fct (atys, rtys)
  | ltc_fkfun (FL.FK_FUN {fixed, ...}, atys, rtys) = 
      ltc_arrow(fixed, atys, rtys)

fun ltd_fkfun lty = 
  if ltp_fct lty then ltd_fct lty
  else let val (_, atys, rtys) = ltd_arrow lty
        in (atys, rtys)
       end

fun ltc_rkind (FL.RK_TUPLE _, lts) = ltc_tuple lts
  | ltc_rkind (FL.RK_STRUCT, lts) = ltc_str lts
  | ltc_rkind (FL.RK_VECTOR t, _) = ltc_vector (ltc_tyc t)

fun ltd_rkind (lt, i) = lt_select (lt, i)

(** a version of ltc_arrow with singleton argument and return result *)
val ltc_arw = ltc_parrow

(** not knowing what FUNCTION this is, to build a fct or an arw *)
fun ltc_fun (x, y) = 
  (case (lt_out x, lt_out y) 
    of (LK.LT_TYC _, LK.LT_TYC _) => ltc_parrow(x, y)
     | _ => ltc_pfct(x, y))

(* lt_arrow used by chkflint.sml, checklty.sml, chkplexp.sml, convert.sml
 * and wrapping.sml only 
 *)
fun lt_arrow t = 
  (case (lt_out t) 
    of (LK.LT_FCT([t1], [t2])) => (t1, t2)
     | (LK.LT_FCT(_, _)) => bug "unexpected case in lt_arrow"
     | (LK.LT_CONT [t]) => (t, ltc_void)
     | _ => (ltd_parrow t) handle _ => 
                bug ("unexpected lt_arrow --- more info: \n\n"
                     ^ (lt_print t) ^ "\n\n")) 

(* lt_arrowN used by flintnm.sml and ltysingle.sml only, should go away soon *)
fun lt_arrowN t = 
  (case (lt_out t) 
    of (LK.LT_FCT(ts1, ts2)) => (ts1, ts2)
     | (LK.LT_CONT ts) => (ts, [])
     | _ => (let val (_, s1, s2) = ltd_arrow t
              in (s1, s2)
             end))

fun tc_upd_prim tc = 
  let fun h(LK.TC_PRIM pt) = 
            if PT.ubxupd pt then PO.UNBOXEDUPDATE
            else if PT.bxupd pt then PO.BOXEDUPDATE 
                 else PO.UPDATE
        | h(LK.TC_TUPLE _ | LK.TC_ARROW _) = PO.BOXEDUPDATE
        | h(LK.TC_FIX ((1,tc,ts), 0)) = 
            let val ntc = case ts of [] => tc
                                   | _ => tcc_app(tc, ts)
             in (case (tc_out ntc)
                  of LK.TC_FN([k],b) => h (tc_out b)
                   | _ => PO.UPDATE)
            end
        | h(LK.TC_SUM tcs) = 
            let fun g (a::r) = if tc_eqv(a, tcc_unit) then g r else false
                  | g [] = true
             in if (g tcs) then PO.UNBOXEDUPDATE else PO.UPDATE
            end
        | h _ = PO.UPDATE
   in h(tc_out tc)
  end


end (* top-level local *)
end (* structure LtyExtern *)

