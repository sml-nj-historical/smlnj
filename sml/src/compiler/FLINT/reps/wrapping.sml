(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* wrapping.sml *)

signature WRAPPING =
sig
  val wrapLexp : Lambda.lexp -> Lambda.lexp

end (* signature WRAPPING *)

structure Wrapping : WRAPPING = 
struct

local structure CO = Coerce
      structure LU = LtyUtil
      structure LT = LtyExtern
      structure DA = Access
      structure DI = DebIndex
      structure PO = PrimOp
      open Lambda
in

fun bug s = ErrorMsg.impossible ("Wrapping: " ^ s)
val say = Control.Print.say

val mkv = LambdaVar.mkLvar
val ident = fn le => le
val IntOpTy = LT.ltc_arw(LT.ltc_tuple[LT.ltc_int,LT.ltc_int],LT.ltc_int)

(****************************************************************************
 *                   MISC UTILITY FUNCTIONS                                 *
 ****************************************************************************)
fun ltApply x = case LT.lt_inst x 
                 of [z] => z
                  | _ => bug "unexpected in ltApply"
fun ltAppSt x = case LT.lt_inst_st x 
                 of [z] => z
                  | _ => bug "unexpected in ltAppSt"
val ltAppSt2 = Stats.doPhase(Stats.makePhase "Compiler 051 2-ltAppSt") ltAppSt
val ltAppSt = Stats.doPhase(Stats.makePhase "Compiler 051 1-ltAppSt") ltAppSt

val ltArrow = LT.lt_arrow 
val ltSelect = LT.lt_select
val ltFun = LT.ltc_fun
val ltTup = LT.ltc_tuple
val lt_eqv = LT.lt_eqv
val tc_eqv = LT.tc_eqv
val tc_real = LT.tcc_real

val lt_upd = 
  let val x = LT.ltc_array (LT.ltc_tv 0)
   in LT.ltc_poly([LT.tkc_mono], 
                  [LT.ltc_arw(LT.ltc_tuple [x, LT.ltc_int, LT.ltc_tv 0], 
                             LT.ltc_unit)])
  end

val lt_sub = 
  let val x = LT.ltc_array (LT.ltc_tv 0)
   in LT.ltc_poly([LT.tkc_mono], 
                  [LT.ltc_arw(LT.ltc_tuple [x, LT.ltc_int], LT.ltc_tv 0)])
  end

datatype primKind = STANDARD | PARRAYOP | RARRAYOP

(*
 * WARN: NEED TO WORK ON GENOP of INLMKARRAY !!!
 *)

fun mkPrim(p as PO.SUBSCRIPT, t, [tc]) = 
      if lt_eqv(t, lt_sub)
      then if tc_eqv(tc, tc_real)
           then (PO.NUMSUBSCRIPT{kind=PO.FLOAT 64, checked=false, 
                                 immutable=false}, RARRAYOP)
           else (p, PARRAYOP)
      else (p, STANDARD)
  | mkPrim(PO.UPDATE, t, [tc]) = 
      if lt_eqv(t,lt_upd)
      then if tc_eqv(tc,tc_real)
           then (PO.NUMUPDATE{kind=PO.FLOAT 64, checked=false}, RARRAYOP)
           else (let val np = LU.tcUpd tc
                  in case np
                      of PO.UPDATE => (np, PARRAYOP)
                       | _ => (np, STANDARD)
                 end)
      else (LU.tcUpd tc, STANDARD)
  | mkPrim(PO.SUBSCRIPT, _, _) = bug "unexpected SUBSCRIPT primops in mkPrim"
  | mkPrim(PO.UPDATE, _, _) = bug "unexpected UPDATE primops in mkPrim"
  | mkPrim(p, _, []) = bug "unexpected arguments in mkPrim"
  | mkPrim(p, _, _) = (p, STANDARD)

(****************************************************************************
 * val transform : CO.wpEnv * LT.ltyEnv * DI.depth                          *
 *                     -> lexp -> (lexp * LT.lty)                           *
 *                                                                          *
 * Transform does the following several things:                             *
 *   (1) representation analysis coercions are inserted;                    *
 *                                                                          *
 *   (2) all conreps in CON and DECON are given type-specific meanings.     *
 *   (3) type abstractions TFN are converted into function abstractions;    *
 *   (4) type applications TAPP are converted into function applications;   *
 *   (5) all primops in PRIM are given type-specific meanings;              *
 *   (6) lty is (narrowed) simplified into those with LT.ltc_void; with     *
 *       the following invariants:                                          *
 *         The resulting lexp is a simply-typed lambda expression, and      *
 *         all explicit type annotations can only be:  ltc_int, ltc_int32,  *
 *         ltc_real, ltc_void, ltc_arw, ltc_tup, or ltc_cont.               * 
 *                                                                          *
 ****************************************************************************)
fun transform (wenv, venv, d) = 
let 

val (tcWrap, ltWrap, tcsWrap) = LU.genWrap true

fun primExp(sv as (PRIM _ | GENOP _), t) = 
      let val x = mkv()
          val (argt,_) = ltArrow t
       in FN(x, argt, APP(sv, VAR x))
      end
  | primExp _ = bug "unexpected cases in primExp"

fun lpve sv = 
  (case sv
    of VAR v => (SVAL sv, LT.ltLookup(venv, v, d))
     | INT i => 
        ((i+i+2; (SVAL sv, LT.ltc_int)) handle Overflow => 
         (let val x = mkv()    
              val z = i div 2
              val (ne,_) = loop(RECORD([INT z,INT (i-z)]))

              (*
               *  The ordering of the above three lines has caused
               *  interesting trap-related instruction-scheduling bug. (zsh)
               *)
           in (LET(x, ne, APP(PRIM(PO.IADD,IntOpTy,[]), VAR x)), LT.ltc_int)
          end))
     | WORD w => 
        let val maxWord = 0wx20000000
         in if Word.<(w, maxWord) then 
              (SVAL sv, LT.ltc_int)
            else let val addu = 
                       PO.ARITH{oper=PO.+, overflow=false, kind=PO.UINT 31}
                     val x1 = Word.div(w, 0w2)
                     val x2 = Word.-(w, x1)
                     val (ne,_) = loop(RECORD [WORD x1, WORD x2])
                     val x = mkv()
                  in (LET(x, ne, APP(PRIM(addu, IntOpTy, []), VAR x)),
                      LT.ltc_int)
                 end
        end
     | (INT32 _ | WORD32 _) => (SVAL sv, LT.ltc_int32)
     | REAL _ => (SVAL sv, LT.ltc_real)
     | STRING _ => (SVAL sv, LT.ltc_string)
     | PRIM (p, lt, []) => (primExp(sv,lt), lt)
     | PRIM (p as (PO.NUMSUBSCRIPT _ | PO.NUMUPDATE _), lt, ts) => 
         (* the polymorphism in NUMSUB & NUMUPD is used for overloading *)
         let val nt = ltAppSt(lt, ts)
          in (primExp (PRIM(p, nt, []), nt), nt)
         end
     | PRIM (p, lt, ts) => 
         let val (np, pknd) = mkPrim(p, lt, ts)
          in (case (tcsWrap ts, pknd)
               of ((_, RARRAYOP) | (NONE, STANDARD)) =>
                    (let val nt = ltAppSt(lt, ts)
                      in (primExp(PRIM(np, nt, []), nt), nt)
                     end)
                | (NONE, PARRAYOP) =>  
                    (** these parrayops are not fully determined ***)
                    (let val nt = ltAppSt(lt, ts)
                      in (primExp(PRIM(np, lt, ts), nt), nt)
                     end)
                | (SOME wts, _) =>
                    (let val nt = ltAppSt(lt, wts)
                         val ot = ltAppSt(lt, ts)
                         val hdr = CO.unwrapOp(wenv, nt, ot, d)
                      in (hdr(primExp(PRIM(np, nt, []), nt)), ot)
                     end))
         end
     | GENOP({default=pv, table=[(_,sv)]}, PO.POLYEQL, _, [tc]) =>
         loop(Equal.equal(pv, sv, tc))
     | GENOP(dict as {default=pv, table=[(_,sv)]}, 
             p as PO.INLMKARRAY, lt, ts as [tc]) =>
         if tc_eqv(tc, tc_real) then 
               let val nt = ltAppSt(lt,ts)
                in (SVAL(VAR sv), nt)
               end
         else (case tcsWrap ts
                of NONE => let val nt = ltAppSt(lt, ts)
                            in (primExp(GENOP(dict, p, lt, ts), nt), nt)
                           end
                 | SOME wts =>  (** we know it cannot be real64 *)
                     let val nt = ltAppSt(lt, wts)
                         val ot = ltAppSt(lt, ts)
                         val hdr = CO.unwrapOp(wenv, nt, ot, d)
                      in (hdr(TAPP(VAR pv, wts)), ot)
                     end)
     | GENOP _ => bug "other GENOPs not implemented yet")

and lpev le = 
  (case le
    of SVAL nsv => (nsv, ident)
     | e => let val v = mkv()
             in (VAR v, fn x => LET(v, e, x))
            end)

and lpsv sv = 
  (case (lpve sv)
    of (SVAL nsv, lt) => (nsv, ident, lt)
     | (e, lt) => 
          let val v = mkv()
           in (VAR v, fn x => LET(v, e, x), lt)
          end)

and loop le = 
  (case le
    of SVAL sv => lpve sv
     | TFN (ks, e) => 
         let val nwenv = CO.wpNew(wenv, d)
             val (ne, nt) = transform (nwenv, venv, DI.next d) e
             val ne' = CO.wpBuild(nwenv, ne)
          in (TFN(ks, ne'), LT.ltc_poly(ks, [nt]))
         end
     | TAPP (v, ts) => 
         let val (nv, hdr0, lt) = lpsv v
          in (case tcsWrap ts
               of NONE => (hdr0(TAPP(nv, ts)), ltApply(lt, ts))
                | SOME nts => 
                    let val nt = ltAppSt2(lt, nts)
                        val ot = ltAppSt2(lt, ts)
                        val hdr = CO.unwrapOp (wenv, nt, ot, d)
                        
                     in (hdr0(hdr(TAPP(nv, nts))), ot)
                    end)
         end
     | PACK (lt, ts, nts, sv) => 
         let val (ne, _) = lpve sv
             val nt = ltAppSt(lt, nts)
             val ot = ltAppSt(lt, ts)
             val hdr = CO.wrapOp (wenv, nt, ot, d)
          in (hdr ne, nt)
         end
     | CON (x as (name, rep, lt), ts, v) => 
         let val (nv, hdr0, _) = lpsv v
             val ot = ltAppSt(lt, ts)
             val (argt, res) = ltArrow(ot)
          in (case ltWrap argt
               of NONE => (hdr0(CON(x, ts, nv)), res)
                | SOME nargt =>
                    let val hdr = CO.wrapOp (wenv, nargt, argt, d)
                        val ne = hdr0(hdr(SVAL nv))
                     in case ne 
                         of SVAL nnv => (CON(x, ts, nnv), res)
                          | _ => 
                             let val z = mkv()
                              in (LET(z, ne, CON(x, ts, VAR z)), res)
                             end
                    end)
         end
     | DECON ((_, DA.CONSTANT _, _), _, _) => (RECORD[], LT.ltc_unit)
         (* reason: unit-carrying data constructors are considered 
                    as constants; *)
     | DECON (x as (name, rep, lt), ts, sv) => 
         let val (nv, hdr0, _) = lpsv sv
             val ot = ltAppSt(lt, ts)
             val (res, argt) = ltArrow(ot)
          in (case ltWrap res
               of NONE => (hdr0(DECON(x, ts, nv)), res)
                | SOME nres =>
                    let val hdr = CO.unwrapOp (wenv, nres, res, d)
                     in (hdr(hdr0(DECON(x, ts, nv))), res)
                    end)
         end
     | SWITCH (v, reps, cases, opp) => 
         let val (nv, hdr0, _) = lpsv v
             fun g (c, x) = (c, #1 (loop x))
             val (ncases, nt) = 
               (case cases
                 of ((c, e)::rest) =>
                      let val (ne, nt) = loop e
                       in ((c, ne)::(map g rest), nt)
                      end
                  | _ => bug "unexpected empty switch cases")
             val nopp = (case opp of NONE => NONE
                                   | SOME x => SOME(#1(loop x)))
          in (hdr0(SWITCH(nv, reps, ncases, nopp)), nt)
         end
     | FN(v, t, e) => 
         let val nvenv = LT.ltInsert(venv, v, t, d)
             val (ne, nt) = transform (wenv, nvenv, d) e
          in (FN(v, t, ne), ltFun(t, nt))
         end
     | FIX(vs, ts, es, eb) => 
         let val nvenv = 
               let fun h (env, v::r, x::z) = h(LT.ltInsert(env, v, x, d), r, z)
                     | h (env, [], []) = env
                     | h _ = bug "unexpected FIX bindings"
                in h(venv, vs, ts)
               end
             val nes = map (fn x => (#1 (transform (wenv, nvenv, d) x))) es
             val (neb, nt) = transform (wenv, nvenv, d) eb
          in (FIX(vs, ts, nes, neb), nt)
         end
     | APP(v1, v2) => 
         let val (nv1, hdr1, nt1) = lpsv v1
             val (nv2, hdr2, _) = lpsv v2
             val (_, nt) = ltArrow nt1
          in (hdr1(hdr2(APP(nv1, nv2))), nt)
         end
     | LET(v, e1, e2) => 
         let val (ne1, nt1) = loop e1
             val nvenv = LT.ltInsert(venv, v, nt1, d)
             val (ne2, nt2) = transform (wenv, nvenv, d) e2
          in (LET(v, ne1, ne2), nt2)
         end

     | RECORD vs => 
         let fun h([], hdr, nvs, nts) = 
                   (hdr(RECORD(rev nvs)), ltTup(rev nts))
               | h(v::r, hdr, nvs, nts) = 
                   let val (nv, h0, nt) = lpsv v
                    in h(r, hdr o h0, nv::nvs, nt::nts)
                   end
          in h(vs, ident, [], [])
         end
     | SRECORD vs => 
         let fun h([], hdr, nvs, nts) = 
                   (hdr(SRECORD(rev nvs)), LT.ltc_str(rev nts))
               | h(v::r, hdr, nvs, nts) = 
                   let val (nv, h0, nt) = lpsv v
                    in h(r, hdr o h0, nv::nvs, nt::nts)
                   end
          in h(vs, ident, [], [])
         end
     | VECTOR (vs, t) => 
         let val (wt, hdr, mhdr) = 
               (case LU.tcWrap t 
                 of NONE => (t, fn sv => SVAL sv, fn le => le) 
                  | SOME z => 
                      let val z' = LT.ltc_tyc z and t' = LT.ltc_tyc t
                          val xh = CO.wrapOp(wenv, z', t', d) 
                          val x = mkv()
                          val y = mkv()
                          val mh = (fn le => LET(x, FN(y, t', xh(SVAL(VAR y))),
                                                 le))
                          val hh = (fn sv => APP(VAR x, sv))
                       in (z, hh, mh) 
                      end)
 
             fun h([], h2, nvs) = h2(VECTOR(rev nvs, wt))
               | h(v::r, h2, nvs) = 
                   let val (xv, h1, _) = lpsv v
                       val (nv, h0) = lpev (hdr xv)
                    in h(r, h2 o h1 o h0, nv::nvs)
                   end
          in (h (vs, mhdr, []), LT.ltc_tyc(LT.tcc_vector t))
         end
     | SELECT (i, v) => 
         let val (nv, hdr, nt) = lpsv v
          in (hdr(SELECT(i, nv)), ltSelect(nt, i))
         end
     | ETAG (v, t) => 
         let val (nv, hdr, _) = lpsv v
          in (hdr(ETAG(nv, t)), LT.ltc_etag t)
         end
     | RAISE (v, t) => 
         let val (nv, hdr, _) = lpsv v
          in (hdr(RAISE(nv, t)), t)
         end
     | HANDLE (e, v) => 
         let val (ne, nt) = loop e
             val (nv, hdr, _) = lpsv v
          in (hdr(HANDLE(ne, nv)), nt)
         end
     | WRAP _ => bug "unexpected WRAP lexp"
     | UNWRAP _ => bug "unexpected UNWRAP lexp")

 in loop 
end (* function transform *)

fun wrapLexp (FN(v, t, e)) = 
      let val wenv = CO.initWpEnv ()
          val venv = LT.initLtyEnv
          val d = DI.top
          val nvenv = LT.ltInsert(venv, v, t, d)
          val (ne, _) = transform (wenv, nvenv, d) e
       in FN(v, t, CO.wpBuild(wenv, ne))
      end
  | wrapLexp _ = bug "unexpected lambda expressions in wrapLexp"

end (* toplevel local *)
end (* structure Wrapping *)

(*
 * $Log: wrapping.sml,v $
 * Revision 1.4  1997/05/05  20:00:18  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.3  1997/04/18  15:40:35  george
 *   Fixing the DECON on the constant data constructor bug reported
 *   by Pichora. -- zsh
 *
 * Revision 1.2  1997/02/26  21:55:31  george
 *    Fixing the incorrect wrapper bug, BUG 1158, reported by Ken Cline
 *    (zcline.sml). This also fixes the core dump bug, BUG 1153,
 *    reported by Nikolaj.
 *
 *)
