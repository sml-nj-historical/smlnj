(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* reify.sml *)

signature REIFY = 
sig
  val reify : FLINT.prog -> FLINT.prog
end (* signature REIFY *)

structure Reify : REIFY = 
struct

local structure LP = TypeOper
      structure LT = LtyExtern
      structure LV = LambdaVar
      structure DA = Access
      structure DI = DebIndex
      structure PO = PrimOp
      structure FU = FlintUtil

      open FLINT
in

fun bug s = ErrorMsg.impossible ("Reify: " ^ s)
val say = Control.Print.say
val mkv = LambdaVar.mkLvar
val ident = fn le => le
fun option f NONE = NONE
  | option f (SOME x) = SOME (f x)

(** a special version of WRAP and UNWRAP for post-reify typechecking *)
val lt_arw = LT.ltc_tyc o LT.tcc_arrow
val lt_vfn = lt_arw(LT.ffc_fixed, [LT.tcc_void], [LT.tcc_void])

fun wty tc = 
  (NONE, PO.WRAP, lt_arw(LT.ffc_fixed, [tc], [LT.tcc_void]), [])
fun uwty tc =   
  (NONE, PO.UNWRAP, lt_arw(LT.ffc_fixed, [LT.tcc_void], [tc]), [])

fun WRAP(tc, vs, v, e) = PRIMOP(wty tc, vs, v, e)
fun UNWRAP(tc, vs, v, e) = PRIMOP(uwty tc, vs, v, e)

(** a major gross hack: use of fct_lty in WCAST primops **)
fun mkWCAST (u, oldt, newt) =
  let val v = mkv()
   in (fn e => PRIMOP((NONE, PO.WCAST, LT.ltc_fct([oldt],[newt]), []), 
                      [u], v, e), v)
  end

fun mcastSingle (oldt, newt) = 
  if LT.lt_eqv(oldt, newt) then NONE
  else SOME (fn u => mkWCAST(u, oldt, newt))

fun mcast (oldts, newts) = 
  let fun f (a::r, b::s, z, flag) = 
              (case mcastSingle(a,b) 
                of NONE => f(r, s, NONE::z, flag)
                 | x => f(r, s, x::z, false))
        | f ([], [], z, flag) = 
              if flag then fn le => le
              else (let val vs = map (fn _ => mkv()) oldts
                        val (hdr, nvs) = 
                          let fun g(NONE::xx, v::yy, h, q) =
                                     g(xx, yy, h, (VAR v)::q)
                                | g((SOME vh)::xx, v::yy, h, q) = 
                                     let val (h', k) = vh (VAR v)
                                      in g(xx, yy, h o h', (VAR k)::q)
                                     end
                                | g([], [], h, q) = (h, rev q)
                                | g _ = bug "unexpected case in mcast"
                           in g(rev z, vs, ident, [])
                          end
                     in fn e => LET(vs, e, hdr(RET nvs))
                    end)
        | f _ = bug "unexpected case in mcast"
   in f(oldts, newts, [], true)
  end

(****************************************************************************
 * Reify does the following several things:                                 *
 *                                                                          *
 *   (1) Conreps in CON and DECON are given type-specific meanings.         *
 *   (2) Type abstractions TFN are converted into function abstractions;    *
 *   (3) Type applications TAPP are converted into function applications;   *
 *   (4) Type-dependent primops such as WRAP/UNWRAP are given               *
 *       type-specific meanings;                                            *
 *   (5) FLINT is now transformed into a monomorphically typed lambda       *
 *       calculus. Type mismatches are fixed via the use of type cast       *
 ****************************************************************************)
(* reify : fundec -> fundec *)
fun reify fdec = 
let val {getLty, cleanUp} =  Recover.recover (fdec, false) 
    val (tcf, ltf, clear) = LT.tnarrow_gen ()

    fun dcf ((name,rep,lt),ts) = (name,rep,lt_vfn)
    fun dargtyc ((name,rep,lt), ts) = 
      let val skt = LT.lt_pinst(lt, map (fn _ => LT.tcc_void) ts)
          val (tc, _) = LT.tcd_parrow (LT.ltd_tyc skt)
          val nt = ltf (LT.lt_pinst(lt, ts))
          val (rt, _) = LT.tcd_parrow (LT.ltd_tyc nt)
       in (tc, rt, (name,rep,lt_vfn))
      end

    (* transform: kenv * DI.depth -> lexp -> lexp *)
    fun transform (kenv, d) = 
     let val getlty = getLty d

         (* lpfd: fundec -> fundec *)
         fun lpfd (fk, f, vts, e) = 
           let val nfk = 
                 case fk 
                  of FK_FUN{isrec=SOME lts, fixed, known, inline} =>
                       FK_FUN{isrec=SOME(map ltf lts), fixed=fixed,
                              known=known, inline=inline}
                   | _ => fk
               val nvts = map (fn (v,t) => (v, ltf t)) vts
            in (nfk, f, nvts, loop e)
           end

         (* lpcon: con -> con * (lexp -> lexp) *) 
         and lpcon (DATAcon(dc as (_, DA.EXN _, nt), [], v)) = 
               let val ndc = dcf(dc, []) and z = mkv() and w = mkv()
                   (* WARNING: the 3rd field should (string list) *) 
                   val (ax,_) = LT.tcd_parrow (LT.ltd_tyc nt)
                   val lt_exr = 
                     LT.tcc_tuple [LT.tcc_void, tcf ax, LT.tcc_int]
                in (DATAcon(ndc, [], z), 
                    fn le => UNWRAP(lt_exr, [VAR z], w, 
                               SELECT(VAR w, 1, v, le)))
               end
           | lpcon (DATAcon(dc as (name, DA.CONSTANT _, lt), ts, v)) = 
               let val ndc = dcf(dc, ts) and z = mkv()
                in (DATAcon(ndc, [], z), 
                    fn le => RECORD(FU.rk_tuple, [], v, le))
               end
           | lpcon (DATAcon(dc as (_, DA.UNTAGGED, _), ts, v)) = 
               let val (tc, rt, ndc) = dargtyc(dc, ts)
                   val hdr = LP.utgd(tc, kenv, rt)
                   val z = mkv()
                in (DATAcon(ndc, [], z),
                    fn le => LET([v], hdr(VAR z), le))
               end
           | lpcon (DATAcon(dc as (_, DA.TAGGED i, _), ts, v)) = 
               let val (tc, rt, ndc) = dargtyc(dc, ts)
                   val hdr = LP.tgdd(i, tc, kenv, rt)
                   val z = mkv()
                in (DATAcon(ndc, [], z),
                    fn le => LET([v], hdr(VAR z), le))
               end
           | lpcon (DATAcon _) = bug "unexpected case in lpcon"
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
                  end
              | TAPP(v, ts) => 
                  let val (u, hdr) = lpev(LP.tsLexp(kenv, ts))

                      (* a temporary hack that fixes type mismatches *)
                      val lt = getlty v
                      val oldts = map ltf (#2 (LT.ltd_poly lt))
                      val newts = map ltf (LT.lt_inst(lt, ts))
                      val nhdr = mcast(oldts, newts)
                   in nhdr (hdr (APP(v, [u])))
                  end
    
              | RECORD(RK_VECTOR tc, vs, v, e) => 
                  RECORD(RK_VECTOR (tcf tc), vs, v, loop e)
              | RECORD(rk, vs, v, e) => RECORD(rk, vs, v, loop e)
              | SELECT(u, i, v, e) => SELECT(u, i, v, loop e)
    
              | CON ((_, DA.CONSTANT i, _), _, _, v, e) => 
                  WRAP(LT.tcc_int, [INT i], v, loop e)

              | CON ((_, DA.EXN (DA.LVAR x), nt), [], u, v, e) => 
                  let val z = mkv()
                      val (ax,_) = LT.tcd_parrow (LT.ltd_tyc nt)
                      val lt_exr = 
                        LT.tcc_tuple [LT.tcc_void, tcf ax, LT.tcc_int]
                   in RECORD(FU.rk_tuple, [VAR x, u, INT 0], z, 
                             WRAP(lt_exr, [VAR z], v, loop e))
                  end

              | CON (dc as (_, DA.UNTAGGED, _), ts, u, v, e) => 
                  let val (tc, rt, _) = dargtyc(dc, ts)
                      val hdr = LP.utgc(tc, kenv, rt)
                   in LET([v], hdr(u), loop e)
                  end
              | CON (dc as (_, DA.TAGGED i, _), ts, u, v, e) => 
                  let val (tc, rt, _) = dargtyc(dc, ts)
                      val hdr = LP.tgdc(i, tc, kenv, rt)
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
    
              | RAISE (u, ts) => RAISE(u, map ltf ts)
              | HANDLE(e, v) => HANDLE(loop e, v)
    
              | BRANCH(xp as (NONE, po, lt, []), vs, e1, e2) => 
                  BRANCH((NONE, po, ltf lt, []), vs, loop e1, loop e2)
              | BRANCH(_, vs, e1, e2) => 
                  bug "type-directed branch primops are not supported"

              | PRIMOP(xp as (_, PO.WRAP, _, _), u, v, e) => 
                  let val tc = FU.getWrapTyc xp
                      val hdr = LP.mkwrp(tc, kenv, true, tcf tc)
                   in LET([v], hdr(RET u), loop e)
                  end
              | PRIMOP(xp as (_, PO.UNWRAP, _, _), u, v, e) =>
                  let val tc = FU.getUnWrapTyc xp
                      val hdr = LP.mkuwp(tc, kenv, true, tcf tc)
                   in LET([v], hdr(RET u), loop e)
                  end
              | PRIMOP(xp as (NONE, po, lt, []), vs, v, e) => 
                  PRIMOP((NONE, po, ltf lt, []), vs, v, loop e)
              | PRIMOP((d, PO.SUBSCRIPT, lt, [tc]), u, v, e) => 
                  let val blt = ltf(LT.lt_pinst(lt, [tc]))
                      val rlt = ltf(LT.lt_pinst(lt, [LT.tcc_real]))
                      val hdr = LP.arrSub(tc, kenv, blt, rlt) 
                   in LET([v], hdr(u), loop e)
                  end
              | PRIMOP((d, po as (PO.UPDATE | PO.UNBOXEDUPDATE 
                                  | PO.BOXEDUPDATE), lt, [tc]), u, v, e) =>
                  let val blt = ltf(LT.lt_pinst(lt, [tc]))
                      val rlt = ltf(LT.lt_pinst(lt, [LT.tcc_real]))
                      val hdr = LP.arrUpd(tc, kenv, po, blt, rlt)
                   in LET([v], hdr(u), loop e)
                  end 
              | PRIMOP((SOME {default=pv, table=[(_,rv)]}, 
                       PO.INLMKARRAY, lt, [tc]), u, v, e) =>
                  let val hdr = LP.arrNew(tc, pv, rv, kenv)
                   in LET([v], hdr(u), loop e)
                  end
              | PRIMOP((_,po,_,_), vs, v, e) => 
                  (say ("\n####" ^ (PrimOp.prPrimop po) ^ "####\n");
                   bug "unexpected PRIMOP in loop"))
      in loop 
     end (* function transform *)

     val (fk, f, vts, e) = fdec
 in (fk, f, map (fn (v,t) => (v, ltf t)) vts,
     transform (LP.initKE, DI.top) e) before (cleanUp(); clear())
end (* function reify *)

end (* toplevel local *)
end (* structure Reify *)

(*
 * $Log$
 *)
