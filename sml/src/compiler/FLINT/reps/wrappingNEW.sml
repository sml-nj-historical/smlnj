(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* wrapping.sml *)

signature WRAPPING =
sig
  val wrapping : FLINT.prog -> FLINT.prog

end (* signature WRAPPING *)

structure Wrapping : WRAPPING = 
struct

local structure CO = Coerce
      structure LT = LtyExtern
      structure DI = DebIndex
      structure PO = PrimOp
      structure DA = Access
      open FLINT
in

fun bug s = ErrorMsg.impossible ("Wrapping: " ^ s)
val say = Control.Print.say
fun mkv _ = LambdaVar.mkLvar()
val ident = fn le => le
fun option f NONE = NONE
  | option f (SOME x) = SOME (f x)

(****************************************************************************
 *                   MISC UTILITY FUNCTIONS                                 *
 ****************************************************************************)
local val lt_upd = 
        let val x = LT.ltc_array (LT.ltc_tv 0)
         in LT.ltc_poly([LT.tkc_mono], 
              [LT.ltc_arrow((true, true), [x, LT.ltc_int, LT.ltc_tv 0], 
                                          [LT.ltc_unit])])
        end
     val lt_sub = 
        let val x = LT.ltc_array (LT.ltc_tv 0)
         in LT.ltc_poly([LT.tkc_mono], 
              [LT.ltc_arrow((true,true), [x, LT.ltc_int], [LT.ltc_tv 0])])
        end
in 

fun isArraySub t = LT.lt_eqv(t, lt_sub)
fun isArrayUpd t = LT.lt_eqv(t, lt_upd)
val f64sub = PO.NUMSUBSCRIPT{kind=PO.FLOAT 64, checked=false, immutable=false} 
val f64upd = PO.NUMUPDATE{kind=PO.FLOAT 64, checked=false}

(* Function classPrim : primop -> primop * bool * bool takes a primop
 * and classifies its kind. It returns a new primop, a flag indicates
 * if this primop has been specialized, and another flag that indicates
 * whether this primop is dependent on runtime type information. (ZHONG)
 *)
fun classPrim(px as (d, p, lt, ts)) =  
  (case (p, ts)
    of ((PO.NUMSUBSCRIPT _ | PO.NUMUPDATE _), _) =>   (* overloaded primops *)
         ((d, p, LT.lt_pinst(lt, ts), []), true, false)
     | (PO.SUBSCRIPT, [tc]) =>                        (* special *)
         if isArraySub lt then
           if LT.tc_eqv(tc, LT.tcc_real)
           then ((d, f64sub, LT.lt_pinst(lt, ts), []), true, false)
           else (px, false, true)
         else (px, false, false)
     | (PO.UPDATE, [tc]) =>                           (* special *)
         if isArrayUpd t then
           if LT.tc_eqv(tc, LT.tcc_real) 
           then ((d, f64upd, LT.lt_pinst(lt, ts), []), true, false)
           else ((d, LT.tc_upd_prim tc, lt, ts), false, true)
         else ((d, LT.tc_upd_prim tc, lt, ts), false, false)
     | _ => (px, false, false))

val argbase = fn vs => (vs, ident)
val resbase = fn v => (v, ident)

end (* utility functions *)

(****************************************************************************
 * The "wrapping" function does the following several things:               *
 *                                                                          *
 *   (1) representation coercions are inserted at TAPP, BRANCH, PRIMOP,     *
 *       CON, SWITCH, and RECORD(RK_VECTOR _, _). Where CON and SWITCH      *
 *       only wrap/unwrap the arguments of a datatype constuctor while      *
 *       RK_VECTOR just wraps the vector elements only.                     *
 *   (2) all primops in PRIM are given type-specific meanings;              *
 *   (3) all conreps in CON and SWITCH are given type-specific meanings ??  *
 *                                                                          *
 ****************************************************************************)
fun wrapping fdec = 
let (* In pass1, we calculate the old type of each variables in the FLINT
     * expression. We do this for the sake of having simpler wrapping code.
     *)
    val {getLty=getLtyGen, cleanUp} = Recover.recover fdec

    (** generate a set of new wrappers *)
    val (tcWrap, ltWrap, tcsWrap) = LU.genWrap true

    fun fixDconTy lt = 
      let fun fix t = 
            (case LT.ltd_arrow t
              of (ff, [aty], rtys) =>
                   (case ltWrap aty 
                     of NONE => t
                      | SOME naty => LT.ltc_arrow(ff, [naty], rtys))
               | _ => bug "unexpected type in fixDconTy")
       in if LT.ltp_ppoly lt then
            let val (ks, t) = LT.ltd_ppoly lt
             in LT.ltc_ppoly(ks, fix t)
            end
          else fix lt
      end (* function fixDconTy *)

    (* transform : CO.wpEnv * DI.depth -> (lexp -> lexp) *)
    fun transform (wenv, d) = 
      let val getlty = getLtyGen d 

          fun lpfd (fk, v, vts, e) = (fk, v, vts, loop e)

          (* lpdc : dcon * tyc list * value * bool -> 
                       (dcon * (lexp -> lexp) * value)  *)
          and lpdc (dc as (name,rep,lt), ts, u, wflag) = 
            let val ot = LT.lt_pinst_st(lt, ts)
                val aty = case LT.ltd_arrow ot 
                           of (_, [x], _) => x
                            | _ => bug "unexpected case in lpdc"
             in case ltWrap aty
                 of NONE => (dc, ident, u)
                  | SOME naty =>
                      let val z = mkv()
                          val nu = VAR z
                          val ndc = (name, rep, fixDconTy lt)
                       in if wflag then  (* CON *)
                            let val hdr = CO.wrapOp(wenv,[naty],[aty],d) 
                             in (fn xe => LET([z], hdr(RET[u]), xe), nu)
                            end   
                          else           (* DECON *)
                            let val hdr = CO.unwrapOp(wenv,[naty],[aty],d)
                                val x = case u 
                                         of VAR q => q
                                          | _ => bug "unexpected case in lpdc"
                             in (fn xe => LET([x], hdr(RET[nu]), xe), nu)
                            end
                      end 
            end (* function lpdc *)

          (* lpsw : con * lexp -> con * lexp *)
          and lpsw (c as DATAcon(dc, ts, v), e) = 
                let val (ndc, hdr, u) = lpdc (dc, ts, VAR v)
                 in (case u 
                      of (VAR nv) => (DATAcon(ndc, ts, nv), hdr(loop e))
                       | _ => bug "unexpected case in lpsw")
                end
            | lpsw (c, e) = (c, loop e)


          (* lprim : primop -> (primop * 
           *                    (value list -> value list * (lexp -> lexp))
           *                    (lvar -> lvar * (lexp -> lexp))) 
           *)
          and lprim(px as (_, _, _, [])) => (px, argbase, resbase)
            | lprim px = 
                let val (npx as (d, np, lt, ts), issp, isdyn) = classPrim px
                 in if issp then  (* primop has been specialized *)
                     (npx, argbase, resbase)
                    else (* still a polymorphic primop *)
                     (case tcsWrap ts
                       of NONE => (* no wrapping is necessary *)
                            if isdyn then (npx, argbase, resbase)
                            else ((d, np, LT.lt_pinst(lt,ts), []), 
                                  argbase, resbase)
                        | SOME wts =>
                            (* does not check isdyn because for the time being,
                               tcsWrap somehow imples that the type cannot be
                               ltc_real any more. pretty adhoc ! (ZHONG) *)
                            let val nt = LT.lt_pinst_st(lt, wts)
                                val (_, nta, ntr) = LT.ltd_arrow nt
                                val ot = LT.lt_pinst_st(lt, ts)
                                val (_, ota, otr) = LT.ltd_arrow ot

                                val hdr1 = CO.wrapOp(wenv, nta, ota, d)
                                fun arghdr vs = 
                                  let val nvs = map mkv vs
                                   in (map VAR nvs, 
                                       fn le => LET(nvs, hdr1(RET vs), le))
                                  end
    
                                val hdr2 = CO.unwrapOp(wenv, ntr, otr, d)
                                fun reshdr v = 
                                  let val nv = mkv()
                                   in (nv, 
                                       fn le => LET([v], hdr2(RET [nv]), le))
                                  end
                             in ((d, np, nt, []), arghdr, reshdr)
                            end)
                end (* function lprim *)

          and loop le = 
            (case le
              of RET _ => le
               | LET (vs, e1, e2) => LET (vs, loop e1, loop e2)
               | FIX (fdecs, e) => FIX(map lpfd fdecs, loop e)
               | APP _ => le
               | TFN ((v, tvks, e1), e2) =>  (* put down all wrappers *)
                   let val nwenv = CO.wpNew(wenv, d)
                       val ne1 = transform (nwenv, DI.next d) e1
                    in (TFN(v, tvks, CO.wpBuild(nwenv, ne1)), loop e2)
                   end
               | TAPP (v, ts) => 
                   (case tcsWrap ts
                     of NONE => le
                      | SOME nts => 
                          let val lt = getlty v
                              val nts = LT.lt_inst(lt, nts)
                              val ots = LT.lt_inst(lt, ts)
                              val hdr = CO.unwrapOp (wenv, nts, ots, d)
                           in hdr(TAPP(v, nts)) 
                          end)
               | CON (dc, ts, u, v, e) => 
                   let val (ndc, hdr, nu) = lpdc(dc, ts, u, true)
                    in hdr (CON(dc, ts, nu, v, loop e))
                   end
               | SWITCH (v, csig, cases, opp) => 
                   SWITCH(v, csig, map lpsw cases, option loop opp)

               | RECORD(RK_VECTOR t, vs, v, e) =>
                   (case tcWrap t
                     of NONE => RECORD(RK_VECTOR t, vs, v, loop e) 
                      | SOME z =>
                          let val z' = LT.ltc_tyc z and t' = LT.ltc_tyc t
                              val xh = CO.wrapOp(wenv, [z'], [t'], d) 
                              val f = mkv() and x = mkv()
                              val fk = FK_FUN{rec=NONE,fixed=(true,true),
                                              known=false, inline=true}
                              fun mh xe = 
                                FIX([(fk,f,[(x,t')],xh(RET[VAR x]))], xe)

                              fun pass([], nvs, h) = 
                                    RECORD(RK_VECTOR z, rev nvs, v, loop e)
                                | pass(u::r, nvs, h) = 
                                    let val z = mkv()
                                        fun h0 xe = 
                                          LET([z], APP(VAR f, [u]), xe)
                                     in pass(r, (VAR z)::nvs, h o h0)
                                    end
                           in pass(vs, [], mh)
                          end)
               | RECORD (rk, vs, v, e) => RECORD(rk, vs, v, loop e)
               | SELECT (u, i, v, e) => SELECT(u, i, v, e)

               | RAISE _ => le
               | HANDLE (e, v) => HANDLE (loop e, v)

               (* resolving the polymorphic equality in a special way *)
               | BRANCH (p as (_, PO.POLYEQL, _, _), vs, e1, e2) = 
                   loop(Equal.equal_branch (p, vs, e1, e2))
               | PRIMOP (p as (_, PO.POLYEQL, _, _), vs, v, e) = 
                   loop(Equal.equal_primop (p, vs, v, e))
 
               (* resolving the polymorphic mkarray *)
               | PRIMOP (p as (d, PO.INLMKARRAY, lt, ts), vs, v, e) = 
                   (case (d, ts)
                     of (SOME {default=pv, table=[(_,sv)]}, [tc]) =>
                         if LT.tc_eqv(tc, LT.tcc_real) then 
                           LET([v], APP(sv, vs), loop e)
                         else 
                           (case tcsWrap ts
                             of NONE => PRIMOP(p, vs, v, loop e)
                              | SOME _ =>  (* can't be real64 anymore *)
                                  let val z = mkv()
                                   in LET([z], loop(TAPP(pv, ts)),
                                        LET([v], APP(VAR z, vs), loop e))
                                  end)
                      | _ => bug "unexpected case for inlmkarray")

               (* resolving the usual primops *)
               | BRANCH (p, vs, e1, e2) => 
                   let val (np, hg, _) = mkPrim p
                       val (nvs, nh) = hg vs
                    in nh(PRIMOP(np, nvs, loop e1, loop e2))
                   end
               | PRIMOP (p, vs, v, e) => 
                   let val (np, hg1, hg2) = mkPrim p
                       val (nvs, nh1) = hg1 vs
                       val (nv, nh2) = hg2 v
                    in nh1(PRIMOP(np, nvs, nv, nh2(loop e)))
                   end)
       in loop 
      end (* function transform *)

    val (fk, f, vts, e) = fdec
 in (fk, f, vts, transform (CO.initWpEnv(), DI.top) e) before (cleanUp())
end (* function wrapping *)

end (* toplevel local *)
end (* structure Wrapping *)
