(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* flintnm.sml *)

(* Converting the Standard PLambda.lexp into the FLINT IL *)
signature FLINTNM = 
sig 
  val norm : PLambda.lexp -> FLINT.fundec
end (* signature FLINTNM *)

structure FlintNM : FLINTNM =
struct 

local structure LT = PLambdaType
      structure FL = PFlatten		(* argument flattening *)
      structure LV = LambdaVar
      structure DI = DebIndex
      structure PT = PrimTyc
      structure L  = PLambda
      structure F  = FLINT
      structure DA = Access
in

val say = Control.Print.say
val mkv = LambdaVar.mkLvar
val ident = fn le : L.lexp => le
fun bug msg = ErrorMsg.impossible("FlintNM: "^msg)

fun optmap f (SOME v)	= SOME (f v)
  | optmap _ NONE	= NONE

fun tocon con =
    let val _ = 1
    in case con of
	L.INTcon x    => F.INTcon x
      | L.INT32con x  => F.INT32con x
      | L.WORDcon x   => F.WORDcon x
      | L.WORD32con x => F.WORD32con x
      | L.REALcon x   => F.REALcon x
      | L.STRINGcon x => F.STRINGcon x
      | L.VLENcon x   => F.VLENcon x
      | L.DATAcon x => bug "unexpected case in tocon"
    end

fun tofundec (venv,d,f_lv,arg_lv,arg_lty,body,isrec) =
    let val (body',body_lty) =
        (* first, we translate the body (in the extended env) *)
        tolexp (LT.ltInsert(venv, arg_lv, arg_lty, d), d) body

        (* detuple the arg type *)
	val (arg_ltys,arg_raw,unflatten,_) = FL.all_flatten arg_lty
            
        (* now, we add tupling code at the beginning of the body *)
        val (arg_lvs, body'') = unflatten(arg_lv, body')

	(* construct the return type if necessary *)
	val (body_ltys,body_raw,_,_) = FL.all_flatten body_lty
	val rettype = if not isrec then NONE
		      else SOME(map FL.ltc_raw body_ltys)

	val isfct = not (LT.ltp_tyc arg_lty andalso LT.ltp_tyc body_lty)
	val f_lty = if isfct then LT.ltc_pfct(arg_lty, body_lty)
		    else LT.ltc_parrow(arg_lty, body_lty)
			
    in (({isrec=rettype, raw=(arg_raw, body_raw), isfct=isfct},
	 f_lv, ListPair.zip(arg_lvs, map FL.ltc_raw arg_ltys), body''),
	f_lty)
    end


(* used to translate expressions whose structure is the same
 * in Flint as in PLambda (either both binding or both non-binding)
 * a continuation is unnecessary *)
and tolexp (venv,d) lexp =
    let fun default_tovalues () =
        tovalues(venv, d, lexp,
                fn (vals, lty) =>
		(F.RET vals, lty))
    in case lexp of
        L.APP (L.PRIM _, arg) => default_tovalues()
      | L.APP (L.GENOP _,arg) => default_tovalues()
      | L.APP (L.FN (arg_lv,arg_lty,body), arg_le) =>
	    tolexp (venv,d) (L.LET(arg_lv, arg_le, body))
      | L.APP (f,arg) =>
            (* first, evaluate f to a mere value *)
            tovalue(venv, d, f,
                    fn (f_val, f_lty) =>
                    (* then eval the argument *)
                    tovalues(venv, d, arg,
			     fn (arg_vals, arg_lty) =>
			     (* now find the return type *)
			     let val (_, r_lty) = LT.ltd_pfun f_lty
			     (* and finally do the call *)
			     in (F.APP(f_val,arg_vals), r_lty)
			     end))

      | L.FIX (lvs,ltys,lexps,lexp) =>
            (* first, let's setup the enriched environment with those funs *)
            let val venv' = foldl (fn ((lv,lty),ve) =>
                                   LT.ltInsert(ve, lv, lty, d))
                                  venv (ListPair.zip(lvs, ltys))

                (* then translate each function in turn *)
                val funs = map (fn ((f_lv,f_lty),L.FN(arg_lv,arg_lty,body)) =>
                                #1(tofundec(venv', d, 
					    f_lv, arg_lv, arg_lty, body, true)))
                               (ListPair.zip(ListPair.zip(lvs,ltys),lexps))

                (* finally, translate the lexp *)
                val (lexp',lty) = tolexp (venv',d) lexp
            in (F.FIX(funs,lexp'), lty)
            end

      | L.LET (lvar,lexp1,lexp2) =>
            tolvar(venv, d, lvar, lexp1,
                   fn lty1 =>
                   tolexp (LT.ltInsert(venv,lvar,lty1,d), d) lexp2)

      | L.TAPP (f,tycs) =>
            (* similar to APP *)
            tovalue(venv, d, f,
                    fn (f_val,f_lty) =>
                    let val r_lty = LT.lt_pinst(f_lty, tycs)
                    in (F.TAPP(f_val, map FL.tcc_raw tycs), r_lty)
                    end)

      | L.RAISE (le, r_lty) => 
            tovalue(venv, d, le,
                    fn (le_val,le_lty) =>
                    let val r_ltys = FL.ltc_flat r_lty
                    in (F.RAISE(le_val, map FL.ltc_raw r_ltys), r_lty)
                    end)

      | L.HANDLE (body, handler) =>
            tovalue(venv, d, handler,
                    fn (h_val,h_lty) =>
                    let val (body', body_lty) = tolexp (venv, d) body
                    in (F.HANDLE(body', h_val), body_lty)
                    end)

      | L.SWITCH (le,acs,[],NONE) => raise Match
	    (* tovalue(venv, d, le, fn _ => (F.RET[], [])) *)
      | L.SWITCH (le,acs,[],SOME lexp) =>
	    tovalue(venv, d, le, fn (v,lty) => tolexp (venv,d) lexp)
      | L.SWITCH (le,acs,conlexps,default) =>
	    let fun f (L.DATAcon((s,cr,lty),tycs,lvar),le) =
		    let val (lv_lty,_) = LT.ltd_parrow(LT.lt_pinst(lty,tycs))
			val newvenv = LT.ltInsert(venv,lvar,lv_lty,d)
			val (le, le_lty) = tolexp (newvenv,d) le
			val (lvars, le) = FL.v_unflatten lv_lty (lvar, le)
		    in
			((F.DATAcon((s,cr,FL.ltc_raw lty),
				    map FL.tcc_raw tycs, lvars),
			  le),
			 le_lty)
		    end
		  | f (con,le) =
		    let val (lexp,lty) = tolexp (venv,d) le
		    in ((tocon con, lexp), lty)
		    end
	    in tovalue(venv, d, le,
		       fn (v, lty) =>
		       let val default = optmap (#1 o tolexp(venv,d)) default
			   val conlexps as ((_,lty)::_) = map f conlexps
		       in (F.SWITCH(v, acs, map #1 conlexps, default), lty)
		       end)
	    end
	    
      (* for mere values, use tovalues *)
      | _ => default_tovalues ()
    end

(*
 * tovalue: turns a PLambda lexp into a value+type and then calls
 * the continuation that will turn it into an Flint lexp+type
 * (ltyenv * DebIndex * L.lexp * ((value * lty) -> (F.lexp * lty list))) -> (F.lexp * lty)
 * 
 * - venv is the type environment for values
 * - conts is the continuation
 *)
and tovalue (venv,d,lexp,cont) =
    let val _ = 1
    in case lexp of
        (* for simple values, it's trivial *)
        L.VAR v => cont(F.VAR v, LT.ltLookup(venv, v, d))
      | L.INT n => cont(F.INT n, LT.ltc_int)
      | L.INT32 n => cont(F.INT32 n, LT.ltc_int32)
      | L.WORD n => cont(F.WORD n, LT.ltc_int)
      | L.WORD32 n => cont(F.WORD32 n, LT.ltc_int32)
      | L.REAL x => cont(F.REAL x, LT.ltc_real)
      | L.STRING s => cont(F.STRING s, LT.ltc_string)

      (* for cases where tolvar is more convenient *)
      | _ => 
            let val lv = mkv()
            in tolvar(venv, d, lv, lexp, fn lty => cont(F.VAR lv, lty))
            end
    end
		    
		    
(*
 * tovalues: turns a PLambda lexp into a list of values and a list of types
 * and then calls the continuation that will turn it into an Flint lexp+type
 *
 * (ltyenv * DebIndex * L.lexp * ((value list * lty list) -> (F.lexp * lty list))) -> (F.lexp * lty)
 * 
 * - venv is the type environment for values
 * - cont is the continuation
 *)
and tovalues (venv,d,lexp,cont) =
    let val _ = 1
    in case lexp of
	L.RECORD (lexps) =>
	    lexps2values(venv,d,lexps,
			 fn (vals,ltys) =>
			 let val lty = LT.ltc_tuple ltys
			     val ltys = FL.ltc_flat lty
			 in
			     (* detect the case where flattening is trivial *)
			     if LT.lt_eqv(lty, LT.ltc_tuple ltys) then
				 cont(vals,lty)
			     else
				 let val lv = mkv()
				     val (vs,wrap) = FL.v_flatten lty (F.VAR lv)
				     val (c_lexp,c_lty) = cont(vs, lty)
				 in
				     (F.RECORD(F.RK_RECORD,
					       vals, lv, wrap c_lexp),
				      c_lty)
				 end
			 end)
			    
      | _ => tovalue(venv,d,lexp,
		     fn (v, lty) =>
		     let val (vs,wrap) = FL.v_flatten lty v
			 val (c_lexp, c_lty) = cont(vs, lty)
		     in (wrap c_lexp, c_lty)
		     end)
    end

(* eval each lexp to a value *)
and lexps2values (venv,d,lexps,cont) =
    let val _ = 1
	fun f [] (vals,ltys) = cont (rev vals, rev ltys)
	  | f (lexp::lexps) (vals,ltys) =
	    tovalue(venv,d,lexp,
		    fn (v, lty) =>
		    f lexps (v::vals, lty::ltys))
    in
	f lexps ([], [])
    end

(*
 * tolvar: same as tovalue except that it binds the value of the PLambda
 * to the indicated lvar and passes just the type to the continutation
 *)
and tolvar (venv,d,lvar,lexp,cont) =
    let fun eta_expand (f, f_lty) =
            let val lv = mkv()
                val (arg_lty, ret_lty) = (LT.ltd_parrow f_lty)
            in tolvar(venv, d, lvar,
                      L.FN(lv, arg_lty, L.APP(f, L.VAR lv)),
                      cont)
            end

        (* inbetween tolvar and tovalue: it binds the lexp to a variable but
         * is free to choose the lvar and passes it to the continutation *)
        fun tolvarvalue (venv,d,lexp,cont) =
            tovalue(venv, d, lexp,
                    fn (v,lty) =>
                    case v of
                        F.VAR lv => cont(lv, lty)
                      | _ => let val lv = mkv()
                                 val (lexp',lty) = cont(lv, lty)
                             in (F.LET ([lv], F.RET [v], lexp'), lty)
                             end)

        fun PO_helper (arg,f_lty,filler) =
            (* first, turn args into values *)
             tovalues(venv, d, arg,
		     fn (arg_vals, arg_lty) =>
		     (* now find the return type(s) *)
		     let val (_, r_lty) = LT.ltd_parrow f_lty
			 (* translate the continutation *)
			 val (c_lexp, c_lty) = cont(r_lty)
		     (* put the filling inbetween *)
		     in (filler(arg_vals, c_lexp), c_lty)
		     end)

        fun default_tolexp () =
            let val (lexp', lty) = tolexp (venv, d) lexp
                val (c_lexp, c_lty) = cont(lty)
                val (lvs,c_lexp') = FL.v_unflatten lty (lvar, c_lexp)
            in (F.LET(lvs, lexp', c_lexp'), c_lty)
            end

(*         fun default_tovalue () = *)
(*             tovalue(venv, d, lexp, *)
(*                     fn (v,lty) => *)
(*                     let val (lexp', ltys) = cont(lty) *)
(*                     in (F.LET([lvar], F.RET[v], lexp'), ltys) *)
(*                     end) *)

    in case lexp of
      (* primops have to be eta-expanded since they're not valid
       * function values anymore in Flint *)
        L.PRIM (po,lty,tycs) => eta_expand(lexp, LT.lt_pinst(lty, tycs))
      | L.GENOP (dict,po,lty,tycs) => eta_expand(lexp, LT.lt_pinst(lty, tycs))

      | L.FN (arg_lv,arg_lty,body) =>
            (* translate the body with the extended env into a fundec *)
            let val (fundec as (fk,f_lv,args,body'), f_lty) =
		    tofundec(venv, d, lvar, arg_lv, arg_lty, body, false)
                val (lexp, lty) = cont(f_lty)
            in (F.FIX([fundec], lexp), lty)
            end

      (* this is were we really deal with primops *)
      | L.APP (L.PRIM ((po,f_lty,tycs)),arg) =>
            PO_helper(arg, LT.lt_pinst(f_lty, tycs),
                       fn (arg_vals,c_lexp) =>
                       F.PRIMOP((po, FL.ltc_raw f_lty, map FL.tcc_raw tycs),
				arg_vals, lvar, c_lexp))

      | L.APP (L.GENOP({default,table},po,f_lty,tycs),arg) =>
            let fun f ([],table,cont) = cont (table)
                  | f ((tycs,le)::t1,t2,cont) =
                tolvarvalue(venv,d,le,
                            fn (le_lv,le_lty) =>
                            f(t1, (map FL.tcc_raw tycs,le_lv)::t2, cont))
            (* first, eval default *)
            in tolvarvalue(venv,d,default,
                           fn (dflt_lv,dflt_lty) =>
                           (* then eval the table *)
                           f(table, [],
                             fn table' =>
                             PO_helper(arg, LT.lt_pinst(f_lty, tycs),
                                        fn (arg_vals,c_lexp) =>
                                        F.GENOP({default=dflt_lv, table=table'},
                                                (po, FL.ltc_raw f_lty, map FL.tcc_raw tycs),
						arg_vals, lvar, c_lexp))))
            end


      | L.TFN (tks, body) =>
            let val (body', body_lty) = tolexp (venv, DI.next d) body
                val lty = LT.ltc_ppoly(tks, body_lty)
                val (lexp', lty) = cont(lty)
            in (F.TFN((lvar, map (fn tk => (mkv(), tk)) tks, body'), lexp'),
                lty)
            end

      | L.ETAG (le,lty) =>
            tovalue(venv, d, le,
                    fn (le_lv, le_lty) =>
                    let val (c_lexp, c_lty) = cont(LT.ltc_etag lty)
                    in (F.ETAG(FL.tcc_raw (LT.ltd_tyc lty), le_lv, 
			       lvar, c_lexp), c_lty)
                    end)
      | L.CON ((s,cr,lty),tycs,le) =>
	    tovalues(venv, d, le,
		     fn (vals,_) =>
		     let val r_lty = LT.lt_pinst(lty, tycs)
			 val (vals,v_lty) = 
                           let val (_,v_lty) = LT.ltd_parrow r_lty
			    in (vals, v_lty)
			   end
			 val (c_lexp, c_lty) = cont(v_lty)
		     in (F.CON((s, cr, FL.ltc_raw lty),
			       map FL.tcc_raw tycs, vals, lvar, c_lexp),
			 c_lty)
		     end)

      | L.VECTOR (lexps,tyc) =>
	   lexps2values(venv,d,lexps,
			fn (vals, ltys) =>
			let val lty = LT.ltc_tyc(LT.tcc_vector tyc)
			    val (c_lexp, c_lty) = cont(lty)
			in (F.RECORD(F.RK_VECTOR (FL.tcc_raw tyc),
				     vals, lvar, c_lexp),
			    c_lty)
			end)
      | L.RECORD lexps =>
	   lexps2values(venv,d,lexps,
			fn (vals, ltys) =>
			let val lty = LT.ltc_tuple ltys
			    val (c_lexp, c_lty) = cont(lty)
			in (F.RECORD(F.RK_RECORD, vals, lvar, c_lexp), c_lty)
			end)
      | L.SRECORD lexps =>
	   lexps2values(venv,d,lexps,
			fn (vals, ltys) =>
			let val lty = LT.ltc_str(ltys)
			    val (c_lexp, c_lty) = cont(lty)
			in (F.RECORD(F.RK_STRUCT, vals, lvar, c_lexp), c_lty)
			end)

      | L.SELECT (n,lexp) =>
	    tovalue(venv, d, lexp,
		    fn (v, lty) =>
		    let val lty = (LT.lt_select(lty, n))
			val (c_lexp, c_lty) = cont(lty)
		    in (F.SELECT(v, n, lvar, c_lexp), c_lty)
		    end)

      | L.PACK (lty,otycs,ntycs,lexp) =>
            bug "PACK is not currently supported"
(*
	    tovalue(venv, d, lexp,
		    fn (v, v_lty) =>
		    let val nlty = LT.lt_pinst(lty, ntycs)
			val (c_lexp, c_lty) = cont(nlty)
		    in (F.PACK(lty,
			       map FL.tcc_raw otycs,
			       map FL.tcc_raw ntycs,
			       v, lvar, c_lexp),
			c_lty)
		    end)
*)

      (* these ones shouldn't matter because they shouldn't appear *)
(*       | L.WRAP _ => bug "unexpected WRAP in plamba" *)
(*       | L.UNWRAP _ => bug "unexpected UNWRAP in plamba" *)

      | _ => default_tolexp ()
    end

fun norm (lexp as L.FN(arg_lv,arg_lty,e)) =
    (#1(tofundec(LT.initLtyEnv, DI.top, mkv(), arg_lv, arg_lty, e, false))
    handle x => raise x)
(*   | norm _ = bug "unexpected toplevel lexp" *)

end (* toplevel local *)
end (* structure FlintNM *)

