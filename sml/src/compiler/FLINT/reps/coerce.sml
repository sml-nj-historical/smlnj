(* Copyright 1996 by Bell Laboratories *)
(* coerce.sml *)

signature COERCE = sig

  type wpEnv

  val initWpEnv : unit -> wpEnv
  val wpNew : wpEnv * DebIndex.depth -> wpEnv
  val wpBuild : wpEnv * Lambda.lexp -> Lambda.lexp

  val unwrapOp : wpEnv * LtyDef.lty * LtyDef.lty * DebIndex.depth
                   -> (Lambda.lexp -> Lambda.lexp) 

  val wrapOp   : wpEnv * LtyDef.lty * LtyDef.lty * DebIndex.depth
                   -> (Lambda.lexp -> Lambda.lexp) 

end (* signature COERCE *)

structure Coerce : COERCE  = 
struct

local structure DI = DebIndex
      structure LT = LtyExtern
      structure LU = LtyUtil
      structure LV = LambdaVar
      open LtyKernel Lambda
in

(****************************************************************************
 *                  UTILITY FUNCTIONS AND CONSTANTS                         * 
 ****************************************************************************) 

fun bug s = ErrorMsg.impossible ("CoerceLexp: " ^ s)
fun say (s : string) = Control.Print.say s

val mkv = LV.mkLvar
val ident = fn le => le

fun split(SVAL v) = (v, ident)
  | split x = let val v = mkv()
               in (VAR v, fn z => LET(v, x, z))
              end

fun APPg(e1, e2) = 
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
   in h1(h2(APP(v1, v2)))
  end

fun RECORDg es = 
  let fun f ([], vs, hdr) = hdr(RECORD (rev vs))
        | f (e::r, vs, hdr) = 
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

fun SRECORDg es = 
  let fun f ([], vs, hdr) = hdr(SRECORD (rev vs))
        | f (e::r, vs, hdr) = 
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

fun WRAPg (z, b, e) = 
  let val (v, h) = split e
   in h(WRAP(z, b, v))
  end

fun UNWRAPg (z, b, e) = 
  let val (v, h) = split e
   in h(UNWRAP(z, b, v))
  end

fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []

fun option(NONE) = false
  | option(SOME _) = true

fun exists(p, a::r) = if p a then true else exists(p, r)
  | exists(p, []) = false

fun opList l = exists(option, l)

fun force (NONE, le) = le
  | force (SOME f, le) = f le

fun minList (a : int, []) = a
  | minList (a, b::r) = if a > b then minList(b, r) else minList(a, r)

(****************************************************************************
 *                           WRAPPER CACHES                                 *
 ****************************************************************************) 
type tpairs = lty * lty
type hdr = lexp -> lexp
type hdrOp = hdr option

type wpCache = (lty * hdrOp) list IntmapF.intmap

val initWpCache : wpCache = IntmapF.empty

(*
 * Warning: because the hash key is not unique, so the following
 * code is problematic. It should be corrected in the future (ZHONG)
 *)
fun wcEnter([], t, x) = bug "unexpected wenv in wcEnter"
  | wcEnter((_, z as ref m)::_, t, x) =
      let val h = lt_key t
       in z := IntmapF.add(m, h, 
                           (t,x)::(IntmapF.lookup m h handle IntmapF => nil))
      end

fun wcLook([], t) = bug "unexpected wenv in wcLook"
  | wcLook((_, z as ref m)::_, t) = 
      (let fun loop((t',x)::rest) = if lt_eqv(t,t') then SOME x else loop rest
             | loop [] = NONE
        in loop(IntmapF.lookup m (lt_key t))
       end handle IntmapF.IntmapF => NONE)

(****************************************************************************
 *                         WRAPPER ENVIRONMENTS                             *
 ****************************************************************************) 
type wpEnv = ((lvar * lexp) list ref * wpCache ref) list
fun initWpEnv () = [(ref [], ref initWpCache)]

fun wpNew(wpEnv, d) = 
  let val od = length wpEnv
      val _ = if (d+1 = od) then () else bug "inconsistent state in wpNew"
   in (ref [], ref initWpCache)::wpEnv
  end

fun wpBuild ([], base) = base
  | wpBuild ((wref,_)::_, base) = 
      foldl (fn ((v, le), b) => LET(v, le, b)) base (!wref)

fun addWrappers(wenv, p, d) = 
  let (** the d value is ignored now but we may use it in the future *)
      val (wref, _) = (hd wenv (* (List.nth(wenv, d)) *)
                       handle _ => bug "unexpected cases in addWrappers")
   in (wref := (p::(!wref)))
  end

(****************************************************************************
 *                            MAIN FUNCTIONS                                *
 ****************************************************************************) 
fun wrapperGen (wflag, sflag) (wenv, nt, ot, d) = 
let 

val doWrap = 
  if sflag then fn exp => let val w = mkv()
			   in addWrappers(wenv, (w,exp), d); SVAL(VAR w)
			  end
  else ident

fun getWTC(wflag, nx, ox, doit) = 
  if tc_eqv(nx, ox) then NONE
  else (if sflag then 
          (let val mark = if wflag then LT.ltc_int else LT.ltc_real (* hack *)
               val key = LT.ltc_str [LT.ltc_tyc nx, LT.ltc_tyc ox, mark]
            in case wcLook(wenv, key)
                of SOME x => x
                 | NONE => (let val res = doit (tc_out nx, tc_out ox)
                             in wcEnter(wenv, key, res); res
                            end)
           end)
        else doit (tc_out nx, tc_out ox))

fun getWLT(wflag, nx, ox, doit) = 
  if lt_eqv(nx, ox) then NONE
  else (if sflag then 
          (let val mark = if wflag then LT.ltc_int else LT.ltc_real (* hack *)
               val key = LT.ltc_str [nx, ox, mark]
            in case wcLook(wenv, key)
                of SOME x => x
                 | NONE => (let val res = doit (lt_out nx, lt_out ox)
                             in wcEnter(wenv, key, res); res
                            end)
           end)
        else doit (lt_out nx, lt_out ox))

fun tcLoop wflag (nx, ox) = 
  getWTC(wflag, nx, ox, 
   (fn (TC_BOX nz, _) => 
          let (* major gross hack mode ON ----- *)
              val nz = case LU.tcWrap ox (* was nz *)
                        of NONE => nz
                         | SOME x => 
                             if tc_eqv(x, nx) then nz 
                             else (case tc_out x of TC_BOX z => z
                                                  | _ => nz)
              (* major gross hack mode OFF ----- *)

              val wp = tcLoop wflag (nz, ox)
              fun hdr le = 
                case wp of NONE => le
                         | SOME _ => force(wp, le) 
           in if wflag then SOME(fn le => WRAPg(nz, true, hdr le))
              else SOME(fn le => hdr(UNWRAPg(nz, true, le)))
          end          
(*
          if tc_eqv(nz, ox) then 
            (if wflag then SOME(fn le => WRAPg(ox, true, le))
             else SOME(fn le => UNWRAPg(ox, true, le)))
          else (say " Type nx is : \n"; say (LT.tc_print nx);
                say "\n Type ox is : \n"; say (LT.tc_print ox); say "\n";
                bug "unexpected TC_BOX in tcLoop")
*)
     | (TC_ABS _, TC_ABS _) => 
          if LT.tc_eqv_bx(nx, ox) then NONE 
          else (say " Type nx is : \n"; say (LT.tc_print nx);
                say "\n Type ox is : \n"; say (LT.tc_print ox); say "\n";
                bug "unexpected abs tycs in tcLoop")

     | (TC_ABS nz, _) => 
          let val nt = LU.tcWrap nz
           in case nt
               of NONE => 
                    if wflag then SOME(fn le => WRAPg(ox, false, le))
                    else SOME(fn le => UNWRAPg(ox, false, le))
                | SOME zz =>
                    (case tc_out zz
                      of TC_BOX nnz => 
                           let val wp = tcLoop wflag (nnz, ox)
                               fun hdr le =
                                 case wp of NONE => le
                                          | SOME _ => force(wp, le) 
                            in if wflag then  
                                 SOME(fn le => WRAPg(nnz, false, hdr le))
                               else SOME(fn le => hdr(UNWRAPg(nnz, false, le)))
                           end
                       | _ => bug "unexpected non-box-tyc in tcLoop")
          end
(*
          if LT.tc_eqv_bx(nz, ox) then 
            (if wflag then SOME(fn le => WRAPg(ox, false, le))
             else SOME(fn le => UNWRAPg(ox, false, le)))
          else (say " Type nx is : \n"; say (LT.tc_print nx);
                say "\n Type ox is : \n"; say (LT.tc_print ox); say "\n";
                bug "unexpected TC_ABS in tcLoop")
*)
     | (TC_TUPLE nxs, TC_TUPLE oxs) => 
          let val wps = ListPair.map (tcLoop wflag) (nxs, oxs)
           in if opList wps then 
                let val v = mkv()
                    val nl = fromto(0, length nxs)
                    val base = map (fn i => SELECT(i, VAR v)) nl
                    val res = ListPair.map force (wps, base)
                    val ax = if wflag then LT.ltc_tyc ox else LT.ltc_tyc nx
                    val e = doWrap(FN(v, ax, RECORDg res))
                 in SOME(fn le => APPg(e, le))
                end
              else NONE
          end
     | (TC_ARROW _, TC_ARROW _) => 
          let val (nx1, nx2) = LU.tcd_arw nx
              val (ox1, ox2) = LU.tcd_arw ox
              val wp1 = tcLoop (not wflag) (nx1, ox1)
              val wp2 = tcLoop wflag (nx2, ox2)
           in (case (wp1, wp2)
                of (NONE, NONE) => NONE
                 | _ => 
                    let val r = mkv() and v = mkv() and w = mkv()
                        val ve = force(wp1, SVAL(VAR v))
                        val re = force(wp2, SVAL(VAR r))
                        val (ax, rx) = if wflag then (ox, nx1) else (nx, ox1)
                        val (ax, rx) = (LT.ltc_tyc ax, LT.ltc_tyc rx)
                        val e = doWrap(FN(w, ax, FN(v, rx,
                                        LET(r, APPg(SVAL(VAR w), ve), re))))
                     in SOME (fn le => APPg(e, le))
                    end)
          end
     | (_, _) => 
          if LT.tc_eqv_bx(nx, ox) then NONE 
          else (say " Type nx is : \n"; say (LT.tc_print nx);
                say "\n Type ox is : \n"; say (LT.tc_print ox); say "\n";
                bug "unexpected other tycs in tcLoop")))

fun ltLoop wflag (nx, ox) = 
  getWLT(wflag, nx, ox, 
   (fn (LT_TYC nz, LT_TYC oz) => tcLoop wflag (nz, oz)
     | (LT_STR nxs, LT_STR oxs) => 
          let val wps = ListPair.map (ltLoop wflag) (nxs, oxs)
           in if opList wps then 
                let val v = mkv()
                    val nl = fromto(0, length nxs)
                    val base = map (fn i => SELECT(i, VAR v)) nl
                    val res = ListPair.map force (wps, base)
                    val ax = if wflag then ox else nx
                    val e = doWrap(FN(v, ax, SRECORDg res))
                 in SOME(fn le => APPg(e, le))
                end
              else NONE
          end
     | (LT_FCT _, LT_FCT _) => 
          let val (nx1, nx2) = 
                case LT.ltd_fct nx of ([a],[b]) => (a,b)
                                    | _ => bug "unexpected LT_FCT"
              val (ox1, ox2) = 
                case LT.ltd_fct ox of ([a],[b]) => (a,b)
                                    | _ => bug "unexpected LT_FCT"
              val wp1 = ltLoop (not wflag) (nx1, ox1)
              val wp2 = ltLoop wflag (nx2, ox2)
           in (case (wp1, wp2)
                of (NONE, NONE) => NONE
                 | _ => 
                    let val r = mkv() and v = mkv() and w = mkv()
                        val ve = force(wp1, SVAL (VAR v))
                        val re = force(wp2, SVAL (VAR r))
                        val (ax, rx) = if wflag then (ox, nx1) else (nx, ox1)
                        val e = doWrap(FN(w, ax, FN(v, rx,
                                         LET(r, APPg(SVAL(VAR w), ve), re))))
                     in SOME (fn le => APPg(e, le))
                    end)
          end
     | (LT_POLY(nks, [nz]), LT_POLY(oks, [oz])) => 
          let val nwenv = wpNew(wenv, d)
              val nd = DI.next d
              val wp = wrapperGen (wflag, sflag) (nwenv, nz, oz, nd)
           in (case wp
                of NONE => NONE
                 | SOME z => 
                    let val nl = fromto(0, length nks) 
                        val ts = map (fn i => LT.tcc_var(DI.innermost, i)) nl
                        val v = mkv() and w = mkv() 
                        val ax = if wflag then ox else nx
                        val we = LET(v, TAPP(VAR w, ts), 
                                     force(wp, SVAL(VAR v)))
                        val nwe = wpBuild(nwenv, we)
                        val e = doWrap(FN(w, ax, TFN(nks, nwe)))
                     in SOME(fn le => APPg(e, le))
                    end)
          end
     | _ => bug "unexpected pair of ltys in ltTrans"))

 in ltLoop wflag (nt, ot)
end (* function wrapperGen *)

(** share or not share ? currently, module wrappers share ! *)
fun sFlag lt = (case (lt_out lt) 
                 of LT_TYC _ => !Control.CG.sharewrap (* was always false *)
                  | _ => true)

fun unwrapOp (wenv, nt, ot, d) = 
  (case (wrapperGen (false, sFlag nt) (wenv, nt, ot, d))
    of NONE => ident
     | SOME wp => 
         let fun h (x as SVAL(VAR _)) = wp(x)
               | h x = let val v = mkv()
                        in LET(v, x, wp(SVAL(VAR v)))
                       end
          in h
         end)

fun wrapOp (wenv, nt, ot, d) = 
  (case (wrapperGen (true, sFlag nt) (wenv, nt, ot, d))
    of NONE => ident
     | SOME wp => 
         let fun h (x as SVAL(VAR _)) = wp(x)
               | h x = let val v = mkv()
                        in LET(v, x, wp(SVAL(VAR v)))
                       end
          in h
         end)

(*
val wrapOp  = 
  fn x => Stats.doPhase(Stats.makePhase "Compiler 056 wrap") (wrapOp x)

val unwrapOp  = 
  fn x => Stats.doPhase(Stats.makePhase "Compiler 057 unwrap") (unwrapOp x)
*)

end (* toplevel local *)
end (* structure Coerce *)


(*
 * $Log: coerce.sml,v $
 * Revision 1.4  1997/08/22  18:39:07  george
 *   Sharing the wrappers for core-language polymorphic functions also.
 *   The sharing can be turned off by setting Compiler.Control.CG.sharewrap
 *   to false.
 *
 * 								-- zsh
 *
 * Revision 1.3  1997/07/15  16:21:25  dbm
 *   Fix representation bug (#1209).
 *
 * Revision 1.2  1997/05/05  20:00:09  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:46  george
 *   Version 109.24
 *
 *)
