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
   * in the first (types) and second (values) argument *)
  val copy : (FLINT.tvar * FLINT.tyc) list ->
             FLINT.lvar IntmapF.intmap ->
             FLINT.lexp -> FLINT.lexp

  val dcon_eq : FLINT.dcon * FLINT.dcon -> bool

end (* signature FLINTUTIL *) 


structure FlintUtil : FLINTUTIL = 
struct

local structure EM = ErrorMsg
      structure LT = LtyExtern
      structure PO = PrimOp
      structure DA = Access
      structure M  = IntmapF
      structure A  = Access
      structure O  = Option
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
fun copy ta alpha le = let

    val tc_subst = LT.tc_nvar_subst_gen()
    val lt_subst = LT.lt_nvar_subst_gen()

    fun substvar alpha lv = ((M.lookup alpha lv) handle M.IntmapF => lv)
    fun substval alpha (VAR lv) = VAR(substvar alpha lv)
      | substval alpha v = v
    fun newv (lv,alpha) =
	let val nlv = cplv lv in (nlv, M.add(alpha,lv,nlv)) end
    fun newvs (lvs,alpha) =
	foldr (fn (lv,(lvs,alpha)) =>
	       let val (nlv,nalpha) = newv(lv,alpha) in (nlv::lvs,nalpha) end)
	      ([],alpha) lvs
    fun cdcon ta alpha (s,ac,lty) =
	(s,
	 case ac
	  of A.EXN(A.LVAR lv) => A.EXN(A.LVAR(substvar alpha lv))
	   | _ => ac,
	 lt_subst ta lty)
    fun cpo ta alpha (dict,po,lty,tycs) =
	(O.map (fn {default,table} =>
		{default=substvar alpha default,
		 table=map (fn (tycs,lv) =>
			    (map (tc_subst ta) tycs, substvar alpha lv))
			   table}) dict,
	 po, lt_subst ta lty, map (tc_subst ta) tycs)
    fun cfk ta {isrec=SOME(ltys,lk),known,inline,cconv} =
	{isrec=SOME(map (lt_subst ta) ltys,lk),
	 known=known, inline=inline, cconv=cconv}
      | cfk _ fk = fk

    fun crk ta (RK_VECTOR tyc) = RK_VECTOR(tc_subst ta tyc)
      | crk _ rk = rk

    fun copy' ta alpha le = let
	val cpo = cpo ta alpha
	val cdcon = cdcon ta alpha
	val substvar = substvar alpha
	val substval = substval alpha
	val copy = copy' ta
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
	       in (cfk ta fk, nf,
		   ListPair.zip(nargs, (map (lt_subst ta o #2) args)),
		   copy nalpha body)
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
	   val ita = (ListPair.map (fn (t,nt) => (t, LT.tcc_nvar nt))
				   (map #1 args, nargs)) @ ta
       in TFN((nlv, ListPair.zip(nargs, map #2 args), copy' ita ialpha body),
		copy nalpha le)
       end
     | TAPP (f,tycs) => TAPP(substval f, map (tc_subst ta) tycs)
     | SWITCH (v,ac,arms,def) =>
       let fun carm (DATAcon(dc,tycs,lv),le) =
	       let val (nlv,nalpha) = newv(lv, alpha)
	       in (DATAcon(cdcon dc, map (tc_subst ta) tycs, nlv),
		   copy nalpha le)
	       end
	     | carm (con,le) = (con, copy alpha le)
       in SWITCH(substval v, ac, map carm arms, Option.map (copy alpha) def)
       end
     | CON (dc,tycs,v,lv,le) =>
       let val (nlv,nalpha) = newv(lv, alpha)
       in CON(cdcon dc, map (tc_subst ta) tycs, substval v, nlv, copy nalpha le)
       end
     | RECORD (rk,vs,lv,le) => 
       let val (nlv,nalpha) = newv(lv, alpha)
       in RECORD(crk ta rk, map substval vs, nlv, copy nalpha le)
       end
     | SELECT (v,i,lv,le) => 
       let val (nlv,nalpha) = newv(lv, alpha)
       in SELECT(substval v, i, nlv, copy nalpha le)
       end
     | RAISE (v,ltys) => RAISE(substval v, map (lt_subst ta) ltys)
     | HANDLE (le,v) => HANDLE(copy alpha le, substval v)
     | BRANCH (po,vs,le1,le2) =>
       BRANCH(cpo po, map substval vs, copy alpha le1, copy alpha le2)
     | PRIMOP (po,vs,lv,le) =>
       let val (nlv,nalpha) = newv(lv, alpha)
       in PRIMOP(cpo po, map substval vs, nlv, copy nalpha le)
       end
    end
in copy' ta alpha le
end


end (* top-level local *)
end (* structure FlintUtil *)

(*
 * $Log$
 *)
