(* COPYRIGHT (c) 1996 Yale FLINT Project *)
(* reify.sml *)

signature REIFY_NEW = 
sig
  val reify : FLINT.prog -> FLINT.prog
end (* signature REIFY *)

structure ReifyNEW : REIFY_NEW = 
struct

local structure LP = TypeOperNEW
      structure LT = LtyExtern
      structure LV = LambdaVar
      structure DA = Access
      structure DI = DebIndex
      structure PO = PrimOp
      structure FU = FlintUtil
      val WRAP = FU.WRAP
      val UNWRAP = FU.UNWRAP
      open FLINT
in

fun bug s = ErrorMsg.impossible ("Reify: " ^ s)
val say = Control.Print.say
val mkv = LambdaVar.mkLvar
val ident = fn le => le
fun option f NONE = NONE
  | option f (SOME x) = SOME (f x)

fun dargtyc (lt, ts) = 
  let val skt = LT.lt_pinst(lt, map (fn _ => LT.tcc_void) ts)
      val (tc, _) = LT.tcd_parrow (LT.ltd_tyc skt)
      val nt = LT.lt_pinst(lt, ts)
      val (rt, _) = LT.tcd_parrow (LT.ltd_tyc nt)
   in (tc, rt)
  end

(****************************************************************************
 * Reify does the following several things:                                 *
 *                                                                          *
 *   (1) Conreps in CON and DECON are given type-specific meanings.         *
 *   (2) Type abstractions TFN are converted into function abstractions;    *
 *   (3) Type applications TAPP are converted into function applications;   *
 *   (4) Type-dependent primops such as WRAP/UNWRAP are given               *
 *       type-specific meanings;                                            *
 *                                                                          *
 ****************************************************************************)
(* reify : fundec -> fundec *)
fun reify fdec = 
let (* transform: kenv * DI.depth -> lexp -> lexp *)
    fun transform (kenv, d) = 
     let (* lpfd: fundec -> fundec *)
         fun lpfd (fk, f, vts, e) = (fk, f, vts, loop e)

         (* lpcon: con -> con * (lexp -> lexp) *) 
         and lpcon (DATAcon(dc as (_, DA.EXN _, nt), [], v)) = 
               let val z = mkv() and w = mkv()
                   val (ax, _) = LT.tcd_parrow(LT.ltd_tyc nt)
                   (* WARNING: the 3rd field should (string list) *) 
                   val nx = LT.tcc_tuple [LT.tcc_etag ax, ax, LT.tcc_int]
                in (DATAcon(dc, [], z), 
                    fn le => UNWRAP(nx, [VAR z], w, 
                               SELECT(VAR w, 1, v, le)))
               end
           | lpcon (DATAcon(dc as (_, DA.UNTAGGED, lt), ts, v)) = 
               let val (tc, rt) = dargtyc(lt, ts)
                   val hdr = LP.utgd(kenv, tc, rt)
                   val z = mkv()
                in (DATAcon(dc, ts, z),
                    fn le => LET([v], hdr(VAR z), le))
               end
           | lpcon (DATAcon(dc as (_, DA.TAGGED i, lt), ts, v)) = 
               let val (tc, rt) = dargtyc(lt, ts)
                   val hdr = LP.tgdd(kenv, i, tc, rt)
                   val z = mkv()
                in (DATAcon(dc, ts, z),
                    fn le => LET([v], hdr(VAR z), le))
               end
           | lpcon (DATAcon(dc as (name, DA.CONSTANT _, lt), ts, v)) = 
               let val z = mkv()
                in (DATAcon(dc, ts, z), 
                    fn le => RECORD(FU.rk_tuple, [], v, le))
               end
           | lpcon (DATAcon((name,_,lt), ts, v)) = 
               bug "unexpected case in lpcon"
           | lpcon c = (c, ident)
    
         (* lpev : lexp -> (value * (lexp -> lexp)) *)
         and lpev (RET [v]) = (v, ident)
           | lpev e = (* bug "lpev not implemented yet" *) 
               let val x= mkv()
                in (VAR x, fn y => LET([x], e, y))
               end
       
         (* loop: lexp -> lexp *)
         and loop le = 
           (case le
             of RET _ => le
              | LET(vs, e1, e2) => LET(vs, loop e1, loop e2)
    
              | FIX(fdecs, e) => FIX(map lpfd fdecs, loop e)
              | APP _  => le
    
              | TFN((v, tvks, e1), e2) => 
                  let val (nkenv, hdr) = LP.tkAbs(kenv, tvks, v)
                      val ne1 = transform (nkenv, DI.next d) e1
                   in hdr(ne1, loop e2)
                      (*** FIX([(fk, v, vts, hdr ne1)], loop e2) ***)
                  end
              | TAPP(v, ts) => 
                  let val (u, hdr) = lpev(LP.tsLexp(kenv, ts))
                   in hdr (APP(v, [u]))
                  end
    
              | RECORD(rk, vs, v, e) => RECORD(rk, vs, v, loop e)
              | SELECT(u, i, v, e) => SELECT(u, i, v, loop e)
    
              | CON ((_, DA.CONSTANT i, _), _, _, v, e) => 
                  WRAP(LT.tcc_int, [INT i], v, loop e)

              | CON ((_, DA.EXN (DA.LVAR x), nt), [], u, v, e) => 
                  let val (ax, _) = LT.tcd_parrow(LT.ltd_tyc nt)
                      (***WARNING: the 3rd field should be string list *)
                      val nx = LT.tcc_tuple [LT.tcc_etag ax, ax, LT.tcc_int]
                      val z = mkv()
                   in RECORD(FU.rk_tuple, [VAR x, u, INT 0], z, 
                             WRAP(nx, [VAR z], v, loop e))
                  end

              | CON ((_, DA.UNTAGGED, lt), ts, u, v, e) => 
                  let val (tc, rt) = dargtyc(lt, ts)
                      val hdr = LP.utgc(kenv, tc, rt)
                   in LET([v], hdr(u), loop e)
                  end
              | CON ((_, DA.TAGGED i, lt), ts, u, v, e) => 
                  let val (tc, rt) = dargtyc(lt, ts)
                      val hdr = LP.tgdc(kenv, i, tc, rt)
                   in LET([v], hdr(u), loop e)
                  end
              | CON (_, ts, u, v, e) => bug "unexpected case CON in loop"

              | SWITCH (v, csig, cases, opp) => 
                  let fun g (c, x) = 
                        let val (nc, hdr) = lpcon c
                         in (nc, hdr(loop x))
                        end
                   in SWITCH(v, csig, map g cases, option loop opp)
                  end
    
              | RAISE _ => le
              | HANDLE(e, v) => HANDLE(loop e, v)
    
              | BRANCH(xp as (_, _, _, []), vs, e1, e2) => 
                  BRANCH(xp, vs, loop e1, loop e2)
              | BRANCH((_, _, _, ts), vs, e1, e2) => 
                  bug "type-directed branch primops are not supported"
              | PRIMOP(xp as (_, _, _, []), vs, v, e) => 
                  PRIMOP(xp, vs, v, loop e)
              | PRIMOP((d, PO.WRAP, lt, [tc]), u, v, e) => 
                  let val hdr = LP.mkwrp(kenv, true, tc)
                   in LET([v], hdr(RET u), loop e)
                  end
              | PRIMOP((d, PO.UNWRAP, lt, [tc]), u, v, e) =>
                  let val hdr = LP.mkuwp(kenv, true, tc)
                   in LET([v], hdr(RET u), loop e)
                  end
              | PRIMOP((d, PO.SUBSCRIPT, lt, [tc]), u, v, e) => 
                  let val hdr = LP.arrSub(kenv, lt, tc)
                   in LET([v], hdr(u), loop e)
                  end
              | PRIMOP((d, po as (PO.UPDATE | PO.UNBOXEDUPDATE 
                                  | PO.BOXEDUPDATE), lt, [tc]), u, v, e) =>
                  let val hdr = LP.arrUpd(kenv, po, lt, tc)
                   in LET([v], hdr(u), loop e)
                  end 
              | PRIMOP((SOME {default=pv, table=[(_,rv)]}, 
                       PO.INLMKARRAY, lt, [tc]), u, v, e) =>
                  let val hdr = LP.arrNew(kenv, lt, tc, pv, rv)
                   in LET([v], hdr(u), loop e)
                  end
              | PRIMOP((_,po,_,_), vs, v, e) => 
                  (say ("\n####" ^ (PrimOp.prPrimop po) ^ "####\n");
                   bug "unexpected PRIMOP in loop"))
      in loop 
     end (* function transform *)

     val (fk, f, vts, e) = fdec
 in (fk, f, vts, transform (LP.initKE, DI.top) e)
end (* function reify *)

end (* toplevel local *)
end (* structure Reify *)
