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

  (* copy a lexp with alpha renaming.
   * free variables remain unchanged except for the renaming specified
   * in the first argument *)
  val copy : FLINT.lvar IntmapF.intmap -> FLINT.lexp -> FLINT.lexp

  val dcon_eq : FLINT.dcon * FLINT.dcon -> bool

end (* signature FLINTUTIL *) 


structure FlintUtil : FLINTUTIL = 
struct

local structure EM = ErrorMsg
      structure LT = LtyExtern
      structure PO = PrimOp
      structure DA = Access
      structure M  = IntmapF
      open FLINT
in 

fun bug msg = EM.impossible("FlintUtil: "^msg)

val rk_tuple : rkind = RK_TUPLE (LT.rfc_tmp)

(* a set of useful primops used by FLINT *)
val tv0 = LT.ltc_tv 0
val btv0 = LT.ltc_tyc(LT.tcc_box (LT.tcc_tv 0))
val etag_lty = 
  LT.ltc_ppoly ([LT.tkc_mono], 
                 LT.ltc_arrow(LT.ffc_rrflint, [LT.ltc_string], 
                                              [LT.ltc_etag tv0]))
fun wrap_lty tc =
  LT.ltc_tyc(LT.tcc_arrow(LT.ffc_fixed, [tc], [LT.tcc_wrap tc]))
fun unwrap_lty tc =
  LT.ltc_tyc(LT.tcc_arrow(LT.ffc_fixed, [LT.tcc_wrap tc], [tc]))

fun mketag tc = (NONE, PO.MKETAG, etag_lty, [tc])
fun wrap tc = (NONE, PO.WRAP, wrap_lty tc, [])
fun unwrap tc = (NONE, PO.UNWRAP, unwrap_lty tc, [])

fun WRAP(tc, vs, v, e) = PRIMOP(wrap tc, vs, v, e)
fun UNWRAP(tc, vs, v, e) = PRIMOP(unwrap tc, vs, v, e)

(* the corresponding utility functions to recover the tyc *)
fun getEtagTyc (_, _, lt, [tc]) = tc
  | getEtagTyc (_, _, lt, []) = 
      let val nt = LT.ltd_tyc (#2(LT.ltd_parrow lt))
       in if LT.tcp_app nt then 
            (case #2 (LT.tcd_app nt)
              of [x] => x
               | _ => bug "unexpected case 1 in getEtagTyc")
          else LT.tcc_void
      end
  | getEtagTyc _ = bug "unexpected case 2 in getEtagTyc"

fun getWrapTyc (_, _, lt, []) = LT.ltd_tyc(#1(LT.ltd_parrow lt))
  | getWrapTyc _ = bug "unexpected case in getWrapTyc"

fun getUnWrapTyc (_, _, lt, []) = LT.ltd_tyc(#2(LT.ltd_parrow lt))
  | getUnWrapTyc _ = bug "unexpected case in getUnWrapTyc"

fun dcon_eq ((s1,c1,t1),(s2,c2,t2)) =
    (s1 = s2) andalso (c1 = c2) andalso LtyBasic.lt_eqv(t1, t2)

val cplv = LambdaVar.dupLvar
(* 
 * general alpha-conversion on lexp free variables remain unchanged
 * except for the renaming specified in the first argument.
 *   val copy: lvar M.intmap -> fundec -> fundec
 *)
fun copy alpha le = let
    fun substvar lv = ((M.lookup alpha lv) handle M.IntmapF => lv)
    fun substval (VAR lv) = VAR(substvar lv)
      | substval v = v
    fun newv (lv,alpha) =
	let val nlv = cplv lv in (nlv, M.add(alpha,lv,nlv)) end
    fun newvs (lvs,alpha) =
	foldr (fn (lv,(lvs,alpha)) =>
	       let val (nlv,nalpha) = newv(lv,alpha) in (nlv::lvs,nalpha) end)
	      ([],alpha) lvs
    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) =
	(s, Access.EXN(Access.LVAR(substvar lv)), lty)
      | cdcon dc = dc
    fun cpo (SOME{default,table},po,lty,tycs) =
	(SOME{default=substvar default,
	      table=map (fn (tycs,lv) => (tycs, substvar lv)) table},
	 po,lty,tycs)
      | cpo po = po
in case le
    of RET vs => RET(map substval vs)
     | LET (lvs,le,body) =>
       let val nle = copy alpha le
	   val (nlvs,nalpha) = newvs(lvs,alpha)
       in LET(nlvs, nle, copy nalpha body)
       end
     | FIX (fdecs,le) =>
       let fun cfun alpha ((fk,f,args,body):fundec,nf) =
	       let val (nargs,nalpha) = newvs(map #1 args, alpha)
	       in (fk, nf, ListPair.zip(nargs, (map #2 args)), copy nalpha body)
	       end
	   val (nfs, nalpha) = newvs(map #2 fdecs, alpha)
	   val nfdecs = ListPair.map (cfun nalpha) (fdecs, nfs)
       in
	   FIX(nfdecs, copy nalpha le)
       end
     | APP (f,args) => APP(substval f, map substval args)
     | TFN ((lv,args,body),le) =>
       (* don't forget to rename the tvar also *)
       let val (nlv,nalpha) = newv(lv,alpha)
	   val (nargs,ialpha) = newvs(map #1 args, nalpha)
       in TFN((nlv, ListPair.zip(nargs, map #2 args), copy ialpha body),
		copy nalpha le)
       end
     | TAPP (f,tycs) => TAPP(substval f, tycs)
     | SWITCH (v,ac,arms,def) =>
       let fun carm (DATAcon(dc,tycs,lv),le) =
	       let val (nlv,nalpha) = newv(lv, alpha)
	       in (DATAcon(cdcon dc, tycs, nlv), copy nalpha le)
	       end
	     | carm (con,le) = (con, copy alpha le)
       in SWITCH(substval v, ac, map carm arms, Option.map (copy alpha) def)
       end
     | CON (dc,tycs,v,lv,le) =>
       let val (nlv,nalpha) = newv(lv, alpha)
       in CON(cdcon dc, tycs, substval v, nlv, copy nalpha le)
       end
     | RECORD (rk,vs,lv,le) => 
       let val (nlv,nalpha) = newv(lv, alpha)
       in RECORD(rk, map substval vs, nlv, copy nalpha le)
       end
     | SELECT (v,i,lv,le) => 
       let val (nlv,nalpha) = newv(lv, alpha)
       in SELECT(substval v, i, nlv, copy nalpha le)
       end
     | RAISE (v,ltys) => RAISE(substval v, ltys)
     | HANDLE (le,v) => HANDLE(copy alpha le, substval v)
     | BRANCH (po,vs,le1,le2) =>
       BRANCH(cpo po, map substval vs, copy alpha le1, copy alpha le2)
     | PRIMOP (po,vs,lv,le) =>
       let val (nlv,nalpha) = newv(lv, alpha)
       in PRIMOP(cpo po, map substval vs, nlv, copy nalpha le)
       end
end


end (* top-level local *)
end (* structure FlintUtil *)

(*
 * $Log$
 *)
