(* Copyright 1997 (c) by YALE FLINT PROJECT *)
(* flintutil.sml *)

signature FLINTUTIL = 
sig
  val rk_tuple : FLINT.rkind

  val mketag : FLINT.tyc -> FLINT.primop
  val wrap   : FLINT.tyc -> FLINT.primop
  val unwrap : FLINT.tyc -> FLINT.primop

  val WRAP   : FLINT.tyc * FLINT.value list 
                         * FLINT.lvar * FLINT.lexp -> FLINT.lexp
  val UNWRAP : FLINT.tyc * FLINT.value list 
                         * FLINT.lvar * FLINT.lexp -> FLINT.lexp

  val getEtagTyc   : FLINT.primop -> FLINT.tyc
  val getWrapTyc   : FLINT.primop -> FLINT.tyc
  val getUnWrapTyc : FLINT.primop -> FLINT.tyc

  val copy : (unit -> FLINT.lvar) -> FLINT.prog -> FLINT.prog
end (* signature FLINTUTIL *) 


structure FlintUtil : FLINTUTIL = 
struct

local structure EM = ErrorMsg
      structure LT = LtyExtern
      structure PO = PrimOp
      structure DA = Access
      open FLINT
in 

fun bug msg = EM.impossible("FlintUtil: "^msg)

val rk_tuple : rkind = RK_TUPLE (LT.default_rflag)

(* a set of useful primops used by FLINT *)
val tv0 = LT.ltc_tv 0
val btv0 = LT.ltc_tyc(LT.tcc_box (LT.tcc_tv 0))
val etag_lty = 
  LT.ltc_ppoly ([LT.tkc_mono],
        LT.ltc_arrow(LT.default_fflag, [LT.ltc_string], [LT.ltc_etag tv0]))
fun wrap_lty tc =
  LT.ltc_tyc(LT.tcc_arrow(LT.default_fflag, [tc], [LT.tcc_box tc]))
fun unwrap_lty tc =
  LT.ltc_tyc(LT.tcc_arrow(LT.default_fflag, [LT.tcc_box tc], [tc]))

fun mketag tc = (NONE, PO.MKETAG, etag_lty, [tc])
fun wrap tc = (NONE, PO.WRAP, wrap_lty tc, [])
fun unwrap tc = (NONE, PO.WRAP, unwrap_lty tc, [])

fun WRAP(tc, vs, v, e) = PRIMOP(wrap tc, vs, v, e)
fun UNWRAP(tc, vs, v, e) = PRIMOP(unwrap tc, vs, v, e)

(* the corresponding utility functions to recover the tyc *)
fun getEtagTyc (_, _, lt, [tc]) = tc
  | getEtagTyc _ = bug "unexpected case 2 in getEtagTyc"

fun getWrapTyc (_, _, lt, []) = LT.ltd_tyc(#1(LT.ltd_parrow lt))
  | getWrapTyc _ = bug "unexpected case in getWrapTyc"

fun getUnWrapTyc (_, _, lt, []) = LT.ltd_tyc(#2(LT.ltd_parrow lt))
  | getUnWrapTyc _ = bug "unexpected case in getUnWrapTyc"

(* 
 * general alpha-conversion on lexp free variables remain unchanged
 *   val copy: (unit -> lvar) -> fundec -> fundec
 *)
fun copy mkLvar = let

    fun look m v = (IntmapF.lookup m v) handle IntmapF.IntmapF => v
    fun rename (lv, m) = 
      let val lv' = mkLvar ()
  	  val m' = IntmapF.add (m, lv, lv')
       in (lv', m')
      end

    fun renamevs (vs, m) = 
      let fun h([], nvs, nm) = (rev nvs, nm)
            | h(a::r, nvs, nm) = 
                 let val (a', nm') = rename(a, nm)
                  in h(r, a'::nvs, nm')
                 end
       in h(vs, [], m)
      end

    fun renamevps (vps, m) = 
      let fun h([], nvs, nm) = (rev nvs, nm)
            | h((a,t)::r, nvs, nm) = 
                 let val (a', nm') = rename(a, nm)
                  in h(r, (a',t)::nvs, nm')
                 end
       in h(vps, [], m)
      end

    (* access *)
    fun ca (DA.LVAR v, m) = DA.LVAR (look m v)
      | ca (DA.PATH (a, i), m) = DA.PATH (ca (a, m), i)
      | ca (a, _) = a

    (* conrep *)
    fun ccr (DA.EXN a, m) = DA.EXN (ca (a, m))
      | ccr (cr, _) = cr

    (* dataconstr *)
    fun cdc ((s, cr, t), m) = (s, ccr (cr, m), t)

    (* con *)
    fun ccon (DATAcon (dc, ts, v), m) = 
          let val (nv, m') = rename(v, m)
           in (DATAcon (cdc(dc, m), ts, nv), m')
          end
      | ccon x = x

    (* dict *)
    fun dict ({default=v, table=tbls}, m) =
      let val nv = look m v
          val ntbls = map (fn (x, v) => (x, look m v)) tbls
       in {default=nv, table=ntbls}
      end

    (* primop *)
    fun cprim (p as (NONE, _, _, _), m) = p
      | cprim ((SOME d, p, lt, ts), m) = (SOME (dict(d, m)), p, lt, ts)

    (* value *)
    fun sv (VAR lv, m) = VAR (look m lv)
      | sv (x as INT _, _) = x
      | sv (x as INT32 _, _) = x
      | sv (x as WORD _, _) = x
      | sv (x as WORD32 _, _) = x
      | sv (x as REAL _, _) = x
      | sv (x as STRING _, _) = x

    (* value list *)
    fun svs (vs, m) = 
      let fun h([], res, m) = rev res
            | h(v::r, res, m) = h(r, (sv(v, m))::res, m)
       in h(vs, [], m)
      end

    (* lexp *)
    fun c (RET vs, m) = RET (svs (vs, m))
      | c (APP (v, vs), m) = APP (sv (v, m), svs (vs, m))
      | c (TAPP (v, ts), m) = TAPP (sv (v, m), ts)
      | c (FIX (fdecs, le), m) = 
           let val (fdecs', nm) = cf(fdecs, m)
            in FIX(fdecs', c(le, nm))
           end
      | c (LET (vs, le1, le2), m) = 
           let val le1' = c(le1, m)
               val (nvs, m') = renamevs(vs, m)
            in LET(nvs, le1', c(le2, m'))
           end
      | c (TFN (tfdec, le), m) = 
           let val (tfdec', nm) = ctf(tfdec, m)
            in TFN(tfdec', c(le, nm))
           end

      | c (SWITCH (v, crl, cel, eo), m) = 
           let fun cc (con, x) = 
                 let val (ncon, m') = ccon (con, m)
                  in (ncon, c (x, m'))
                 end
    	       fun co NONE = NONE
  	         | co (SOME x) = SOME (c (x, m))
 	    in SWITCH (sv (v, m), crl, map cc cel, co eo)
  	   end
      | c (CON (dc, ts, u, v, le), m) = 
           let val (nv, nm) = rename(v, m)
            in CON (cdc (dc, m), ts, sv (u, m), nv, c(le, nm))
           end
      | c (RECORD (rk, vs, v, le), m) = 
           let val (nv, nm) = rename(v, m)
            in RECORD (rk, svs (vs, m), nv, c(le, nm))
           end
      | c (SELECT (u, i, v, le), m) = 
           let val (nv, nm) = rename(v, m)
            in SELECT (sv (u,m), i, nv, c(le, nm))
           end
      | c (RAISE (v, ts), m) = RAISE (sv (v, m), ts)
      | c (HANDLE (e, v), m) = HANDLE (c (e, m), sv (v, m))
      | c (BRANCH (p, vs, e1, e2), m) = 
           BRANCH (cprim(p, m), svs(vs, m), c(e1, m), c(e2, m))
      | c (PRIMOP (p, vs, v, le), m) = 
           let val (nv, nm) = rename(v, m)
            in PRIMOP(cprim(p,m), svs(vs, m), nv, c(le, nm))
           end

    and ctf ((v,args,le), m) = 
      let val (nv, nm) = rename(v, m)
          (*** ZSH-WARNING: I didn't bother to rename tvars in args ***)
       in ((nv, args, c(le, m)), nm)
      end

    and cf (fdecs, m) =
      let fun pass1([], res, m) = (rev res, m)
            | pass1((_, v, _, _)::r, res, m) = 
                let val (nv, nm) = rename(v, m)
                 in pass1(r, nv::res, nm)
                end

          val (nvs, nm) = pass1(fdecs, [], m)

          fun pass2([], [], res) = (rev res, nm)
            | pass2((fk, _, args, le)::r, nv::nvs, res) = 
                let val (args', nm') = renamevps(args, nm)
                 in pass2(r, nvs, (fk, nv, args', c(le, nm'))::res)
                end
            | pass2 _ = bug "unexpected cases in cf - pass2"
       in pass2(fdecs, nvs, [])
      end
in
    fn fdec => 
      let val init = IntmapF.empty
          val (fdecs', _) = cf([fdec], init)
       in (case fdecs' 
            of [x] => x
             | _ => bug "unexpected cases in copy - top")
      end
end (* function copy *)

end (* top-level local *)
end (* structure FlintUtil *)
