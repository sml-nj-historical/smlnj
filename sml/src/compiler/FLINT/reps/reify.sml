(* COPYRIGHT (c) 1996 Yale FLINT Project *)
(* reify.sml *)

signature REIFY = 
sig
  val ltyComp : Lambda.lexp -> Lambda.lexp
end (* signature LTYCOMP *)

structure Reify : REIFY = 
struct

local structure LP = TypeOper
      structure LT = LtyExtern
      structure LU = LtyUtil
      structure LV = LambdaVar
      structure DA = Access
      structure DI = DebIndex
      structure PO = PrimOp
      open Lambda
in

fun bug s = ErrorMsg.impossible ("Reify: " ^ s)
val mkv = LambdaVar.mkLvar
val ident = fn le => le

fun ltAppSt (lt, ts) = 
  (case LT.lt_inst(lt, ts) 
    of [b] => b 
     | _ => bug "unexpected case in ltAppSt")

fun split(SVAL v) = (v, ident)
  | split x = let val v = mkv()
               in (VAR v, fn z => LET(v, x, z))
              end

fun APPg(e1, e2) = 
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
   in h1(h2(APP(v1, v2)))
  end

fun SELECTg(i, e) = 
  let val (v, hdr) = split e
   in hdr(SELECT(i, v))
  end

fun WRAPg (z, b, e) = 
  let val (v, hdr) = split e
   in hdr(WRAP(z, b, v))
  end

fun RECORDg es = 
  let fun f ([], vs, hdr) = hdr(RECORD (rev vs))
        | f (e::r, vs, hdr) = 
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

(* val exnLexp : DA.access -> lexp *)
fun exnLexp (DA.LVAR v) = SVAL(VAR v)
(*  | exnLexp (DA.PATH(r, i)) = SELECTg(i, exnLexp r) *)
  | exnLexp _ = bug "unexpected case in exnLexp"

(****************************************************************************
 * val transform : kenv * DI.depth -> lexp -> lexp                          *
 *                                                                          *
 * Transform does the following several things:                             *
 *   (1) Conreps in CON and DECON are given type-specific meanings.         *
 *   (2) Type abstractions TFN are converted into function abstractions;    *
 *   (3) Type applications TAPP are converted into function applications;   *
 *   (4) WRAP/UNWRAP are given type-specific meanings;                      *
 *   (?) lty is (narrowed) simplified into those with LT.ltc_void; with     *
 *       the following invariants:                                          *
 *         The resulting lexp is a simply-typed lambda expression, and      *
 *         all explicit type annotations can only be:  ltc_int, ltc_int32,  *
 *         ltc_real, ltc_void, ltc_arw, ltc_tup, or ltc_cont.               * 
 *                                                                          *
 ****************************************************************************)
fun transform (kenv, d) = 
let 

fun lpsv sv = 
  (case sv
    of VAR v => sv
     | (INT _ | WORD _ | INT32 _ | WORD32 _ | REAL _ | STRING _) => sv
     | PRIM _ => sv
     | _ => bug "unexpected value in lpsv in transform")

fun loop le = 
  (case le
    of SVAL sv => SVAL(lpsv sv)
     | TFN (ks, e) => 
         let val (nkenv, hdr) = LP.tkLexp(kenv, ks)
             val ne = transform (nkenv, DI.next d) e
          in hdr ne
         end

     | TAPP (v, ts) => APPg(SVAL(lpsv v), LP.tsLexp(kenv, ts))

     | WRAP(tc, b, v) => 
         let val hdr = LP.mkwrp(kenv, b, tc)
          in hdr(SVAL(lpsv v))
         end
     | UNWRAP(tc, b, v) => 
         let val hdr = LP.mkuwp(kenv, b, tc)
          in hdr(SVAL(lpsv v))
         end

     | CON ((_, DA.UNTAGGED, lt), ts, v) => 
         let val nt = ltAppSt(lt, map (fn _ => LT.tcc_void) ts)
             val (ntc, _) = LT.tcd_parrow(LT.ltd_tyc nt)
(*
             val ntc = case LU.tcWrap tc of NONE => tc
                                          | SOME z => z
*)
             val hdr = LP.utgc(kenv, ntc)
          in hdr (SVAL(lpsv v))
         end
     | DECON ((_, DA.UNTAGGED, lt), ts, v) => 
         let val nt = ltAppSt(lt, map (fn _ => LT.tcc_void) ts)
             val (ntc, _) = LT.tcd_parrow(LT.ltd_tyc nt)
(*
             val ntc = case LU.tcWrap tc of NONE => tc
                                          | SOME z => z
*)
             val hdr = LP.utgd(kenv, ntc)
          in hdr (SVAL(lpsv v))
         end

     | CON ((_, DA.TAGGED i, lt), ts, v) => 
         let val nt = ltAppSt(lt, map (fn _ => LT.tcc_void) ts)
             val (ntc, _) = LT.tcd_parrow(LT.ltd_tyc nt)
(*
             val ntc = case LU.tcWrap tc of NONE => tc
                                          | SOME z => z
*)
             val hdr = LP.tgdc(kenv, i, ntc)
          in hdr (SVAL(lpsv v))
         end
     | DECON ((_, DA.TAGGED i, lt), ts, v) => 
         let val nt = ltAppSt(lt, map (fn _ => LT.tcc_void) ts)
             val (ntc, _) = LT.tcd_parrow(LT.ltd_tyc nt)
(*
             val ntc = case LU.tcWrap tc of NONE => tc
                                          | SOME z => z
*)
             val hdr = LP.tgdd(kenv, i, ntc)
          in hdr (SVAL(lpsv v))
         end

     | CON ((_, DA.CONSTANT i, _), _, _) => WRAP(LT.tcc_int, true, INT i)

     | DECON ((_, DA.CONSTANT _, _), _, _) => 
         bug "DECON on a constant data constructor"

     | CON ((_, DA.EXN p, nt), [], v) => 
         let val (nax, _) = LT.tcd_parrow(LT.ltd_tyc nt)
             (***WARNING: the type of ax is adjusted to reflect boxing *)
(*
             val nax = case LU.tcWrap ax of NONE => ax
                                          | SOME z => z
*)
             (***WARNING: the type for the 3rd field should (string list) *)
             val nx = LT.tcc_tuple [LT.tcc_etag nax, nax, LT.tcc_int]
             
          in WRAPg(nx, true, RECORDg [exnLexp p, SVAL(lpsv v), SVAL(INT 0)])
         end
     | DECON ((_, DA.EXN _, nt), [], v) => 
         let val (nax, _) = LT.tcd_parrow(LT.ltd_tyc nt)
             (***WARNING: the type of ax is adjusted to reflect boxing *)
(*
             val nax = case LU.tcWrap ax of NONE => ax
                                          | SOME z => z
*)
             (***WARNING: the type for the 3rd field should (string list) *)
             val nx = LT.tcc_tuple [LT.tcc_etag nax, nax, LT.tcc_int]
          in SELECTg(1, UNWRAP(nx, true, lpsv v))
         end

     | CON ((_, DA.TRANSPARENT, lt), [], v) =>
         bug "CON-tnsp current not implemented"
     | DECON ((_, DA.TRANSPARENT, lt), [], v) =>
         bug "DECON-tnsp current not implemented"
     | CON ((_, DA.REF, lt), [], v) =>
         bug "CON-ref unexpected in ltyComp"
     | DECON ((_, DA.REF, lt), [], v) =>
         bug "DECON-ref unexpected in ltyComp"
     | CON _ => bug "unexpected CON in transform"
     | DECON _ => bug "unexpected DECON in transform"

     | SWITCH (v, reps, cases, opp) => 
         let fun g (c, x) = (c, loop x)
             fun h (NONE) = NONE
               | h (SOME x) = SOME(loop x)
          in SWITCH(lpsv v, reps, map g cases, h opp)
         end

     | FN(v, t, e) => FN(v, t, loop e)
     | FIX(vs, ts, es, eb) => FIX(vs, ts, map loop es, loop eb)
     | APP(PRIM(PO.SUBSCRIPT, lt, [tc]), v) => 
         let val hdr = LP.arrSub(kenv, lt, tc)
          in hdr(lpsv v)
         end
     | APP(PRIM(PO.UPDATE, lt, [tc]), v) =>
         let val hdr = LP.arrUpd(kenv, lt, tc)
          in hdr(lpsv v)
         end
     | APP(GENOP({default=pv, table=[(_,rv)]}, PO.INLMKARRAY, lt, [tc]), v) =>
         let val hdr = LP.arrNew(kenv, lt, tc, pv, rv)
          in hdr(lpsv v)
         end
     | APP(v1, v2) => APP(lpsv v1, lpsv v2)
     | LET(v, e1, e2) => LET(v, loop e1, loop e2)
     | RECORD vs => RECORD(map lpsv vs)
     | SRECORD vs => SRECORD(map lpsv vs)
     | VECTOR (vs, t) => VECTOR(map lpsv vs, t)   
     | SELECT (i, v) => SELECT(i, lpsv v)

     (* I'd like to make the following concrete in the future *)
     | ETAG (v, t) => ETAG(lpsv v, t)         (* t is always monomorphic *)

     | RAISE (v, t) => RAISE(lpsv v, t)       (* t is always monomorphic *)
     | HANDLE (e, v) => HANDLE(loop e, lpsv v)

     | PACK _ => bug "unexpected PACK lexp in ltyComp")

 in loop 
end (* function transform *)

fun ltyComp le = transform (LP.initKE, DI.top) le

end (* toplevel local *)
end (* structure Reify *)


(*
 * $Log: ltycomp.sml,v $
 * Revision 1.3  1997/05/05  20:00:12  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.2  1997/02/26  21:53:45  george
 *    Fixing the incorrect wrapper bug, BUG 1158, reported by Ken Cline
 *    (zcline.sml). This also fixes the core dump bug, BUG 1153,
 *    reported by Nikolaj.
 *
 *)
