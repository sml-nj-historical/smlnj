(* Copyright 1998 YALE FLINT PROJECT *)
(* typeoper.sml *)

signature TYPEOPER = 
sig
  type kenv
  type tkind = LtyDef.tkind
  type tyc   = LtyDef.tyc
  type lty   = LtyDef.lty
  type tvar  = LtyDef.tvar
  type lvar  = LambdaVar.lvar
  type lexp  = FLINT.lexp
  type value = FLINT.value

  val initKE : kenv
  val tkAbs  : kenv * (tvar * tkind) list * lvar -> 
                  (kenv * (lexp * lexp -> lexp))
  val tcLexp : kenv -> tyc -> lexp
  val tsLexp : kenv * tyc list -> lexp 

  val utgc   : tyc * kenv * tyc -> value -> lexp 
  val utgd   : tyc * kenv * tyc -> value -> lexp 
  val tgdc   : int * tyc * kenv * tyc -> value -> lexp 
  val tgdd   : int * tyc * kenv * tyc -> value -> lexp 

  val mkwrp  : tyc * kenv * bool * tyc -> lexp -> lexp 
  val mkuwp  : tyc * kenv * bool * tyc -> lexp -> lexp 

  val arrSub : tyc * kenv * lty * lty -> value list -> lexp
  val arrUpd : tyc * kenv * PrimOp.primop * lty * lty -> value list -> lexp
  val arrNew : tyc * lvar * lvar * kenv -> value list -> lexp

end (* signature TYPEOPER *)

structure TypeOper : TYPEOPER = 
struct

local structure DI = DebIndex
      structure LT = LtyExtern
      structure LV = LambdaVar
      structure PO = PrimOp
      structure PT = PrimTyc
      structure BT = BasicTypes 
      structure TP = Types
      open LtyKernel FLINT RuntimeType
in

type tkind = tkind
type tyc   = tyc
type lty   = lty
type tvar  = LtyDef.tvar
type lvar  = LV.lvar
type lexp  = lexp
type value = value

fun bug s = ErrorMsg.impossible ("LtyPrim: " ^ s)
fun say (s : string) = Control.Print.say s
fun mkv _ = LV.mkLvar()
val ident = fn le => le
val fkfun = {isrec=NONE, known=false, inline=IH_ALWAYS, cconv=CC_FUN LT.ffc_fixed}
val fkfct = {isrec=NONE, known=false, inline=IH_SAFE, cconv=CC_FCT}

fun mkarw(ts1, ts2) = LT.tcc_arrow(LT.ffc_fixed, ts1, ts2)

val lt_arw = LT.ltc_tyc o LT.tcc_arrow
fun wty tc = 
  (NONE, PO.WRAP, lt_arw(LT.ffc_fixed, [tc], [LT.tcc_void]), [])
fun uwty tc =   
  (NONE, PO.UNWRAP, lt_arw(LT.ffc_fixed, [LT.tcc_void], [tc]), [])

fun FU_WRAP(tc, vs, v, e) = PRIMOP(wty tc, vs, v, e)
fun FU_UNWRAP(tc, vs, v, e) = PRIMOP(uwty tc, vs, v, e)
val FU_rk_tuple = FlintUtil.rk_tuple

fun WRAP(t, u) = 
  let val v = mkv() 
   in FU_WRAP(t, [u], v, RET[VAR v]) 
  end

fun UNWRAP(t, u) = 
  let val v = mkv() 
   in FU_UNWRAP(t, [u], v, RET[VAR v]) 
  end

(****************************************************************************
 *                  UTILITY FUNCTIONS AND CONSTANTS                         * 
 ****************************************************************************) 
fun split(RET [v]) = (v, ident)
  | split x = let val v = mkv()
               in (VAR v, fn z => LET([v], x, z))
              end

fun SELECTg(i, e) = 
  let val (v, hdr) = split e
      val x = mkv()
   in hdr(SELECT(v, i, x, RET [VAR x]))
  end

fun FNg(vts, e) = 
  let val f = mkv()
   in FIX([(fkfun, f, vts, e)], RET[VAR f])
  end

fun SELECTv(i, u) = 
  let val x = mkv()
   in SELECT(u, i, x, RET [VAR x])
  end

fun APPg(e1, e2) = 
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
   in h1(h2(APP(v1, [v2])))
  end

fun RECORDg es = 
  let fun f ([], vs, hdr) = 
               let val x = mkv()
                in hdr(RECORD(FU_rk_tuple, rev vs, x, RET[VAR x]))
               end
        | f (e::r, vs, hdr) = 
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

fun SRECORDg es = 
  let fun f ([], vs, hdr) = 
               let val x = mkv()
                in hdr(RECORD(RK_STRUCT, rev vs, x, RET[VAR x]))
               end
        | f (e::r, vs, hdr) = 
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

fun WRAPg (z, b, e) = 
  let val (v, h) = split e
   in h(WRAP(z, v))
  end

fun UNWRAPg (z, b, e) = 
  let val (v, h) = split e
   in h(UNWRAP(z, v))
  end

fun WRAPcast (z, b, e) = 
  let val (v, h) = split e
      val pt = LT.ltc_arrow(LT.ffc_fixed, [LT.ltc_tyc z], [LT.ltc_void])
      val pv = (NONE,PO.CAST,pt,[])
      val x = mkv()
   in h(PRIMOP(pv, [v], x, RET[VAR x]))
  end

fun UNWRAPcast (z, b, e) = 
  let val (v, h) = split e
      val pt = LT.ltc_arrow(LT.ffc_fixed, [LT.ltc_void], [LT.ltc_tyc z])
      val pv = (NONE,PO.CAST,pt,[])
      val x = mkv()
   in h(PRIMOP(pv, [v], x, RET[VAR x]))
  end

fun SWITCHg (e, s, ce, d) = 
  let val (v, h) = split e
   in h(SWITCH(v, s, ce, d))
  end

fun COND(u,e1,e2) = u(e1,e2)


(****************************************************************************
 *                           KIND ENVIRONMENTS                              *
 ****************************************************************************) 
type kenv = (LV.lvar list * tkind list) list

val initKE = []
fun addKE(kenv, vs, ks) = (vs,ks)::kenv
fun vlookKE(kenv, i, j) = 
  let val (vs,_) = (List.nth(kenv, i-1) 
		     handle _ => bug "unexpected case1 in vlookKE")
   in ((List.nth(vs, j) handle _ => bug "unexpected case2 in vlookKE"))
  end

fun klookKE(kenv, i, j) = 
  let val (_,ks) = (List.nth(kenv, i-1) 
		     handle _ => bug "unexpected case1 in klookKE")
   in ((List.nth(ks, j) handle _ => bug "unexpected case2 in klookKE"))
  end

(****************************************************************************
 *                            MAIN FUNCTIONS                                *
 ****************************************************************************)

(* val tkAbsGen : kenv * lvar list * tkind list * lvar * fkind 
                  -> kenv * ((lexp *lexp) -> lexp) *)
fun tkAbsGen (kenv, vs, ks, f, fk) = 
  let val mkArgTy = case fk of {cconv=CC_FUN _,...} => LT.ltc_tuple
                             | {cconv=CC_FCT,...} => LT.ltc_str
      val argt = mkArgTy (map LT.tk_lty ks)

      val w = mkv()
      fun h([], i, base) = base
	| h(v::r, i, base) = h(r, i+1, SELECT(VAR w, i, v, base))
      
      fun hdr (e1, e2) = FIX([(fk, f, [(w, argt)], h(vs,0,e1))], e2)
   in (addKE(kenv, vs, ks), hdr)
  end

(* val tkAbs: kenv * (tvar * tkind) list -> kenv * (lexp * lexp -> lexp) *)
fun tkAbs (kenv, tvks, f) = 
  let val (vs, ks) = ListPair.unzip tvks
   in tkAbsGen(kenv, vs, ks, f, fkfct)
  end

(* val tkTfn: kenv * tkind list -> kenv * (lexp -> lexp) *)
fun tkTfn (kenv, ks) = 
  let val vs = map (fn _ => mkv ()) ks
      val f = mkv()
      val (nkenv, hdr) = tkAbsGen(kenv, vs, ks, f, fkfun)
   in (nkenv, fn e => hdr(e, RET[VAR f]))
  end

val intty = LT.ltc_int
val boolty = (* LT.ltc_bool *) LT.ltc_void
val inteqty = LT.ltc_arrow(LT.ffc_fixed, [intty, intty], [boolty])
val intopty = LT.ltc_arrow(LT.ffc_fixed, [intty, intty], [intty])
val ieqprim = (NONE, PO.IEQL, inteqty, [])
val iaddprim = (NONE, PO.IADD, intopty, [])
fun ieqLexp (e1, e2) = 
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
   in fn (te, fe) => h1(h2(BRANCH(ieqprim, [v1,v2], te, fe)))
  end
fun iaddLexp (e1, e2) = 
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
      val x = mkv () 
   in h1(h2(PRIMOP(iaddprim, [v1,v2], x, RET[VAR x])))
  end

val tolexp = fn tcode => RET[tovalue tcode]
val tcode_void   : lexp = tolexp tcode_void
val tcode_record : lexp = tolexp tcode_record
val tcode_int32  : lexp = tolexp tcode_int32
val tcode_pair   : lexp = tolexp tcode_pair
val tcode_fpair  : lexp = tolexp tcode_fpair
val tcode_real   : lexp = tolexp tcode_real
val tcode_realN  : int -> lexp = fn i => tolexp (tcode_realN i)

datatype outcome 
  = YES
  | NO
  | MAYBE of lexp  

(* tcLexp maps TC_VAR to proper lvars, TC_PRIM to proper constants *)
(* val tcLexp : kenv -> tyc -> lexp *)
fun tcLexp (kenv : kenv) (tc : tyc) = 
  let fun loop (x : tyc) = 
	(case (tc_out x)
	  of (TC_FN(ks, tx)) => 
		let val (nenv, hdr) = tkTfn(kenv, ks)
		 in hdr(tcLexp nenv tx)
		end
	   | (TC_APP(tx, ts)) => 
		(case tc_out tx
		  of (TC_APP _ | TC_PROJ _ | TC_VAR _) => 
			APPg(loop tx, tcsLexp(kenv, ts))
		   | _ => tcode_void)
	   | (TC_SEQ ts) => tcsLexp(kenv, ts)
	   | (TC_PROJ(tx, i)) => SELECTg(i, loop tx)
	   | (TC_PRIM pt) => 
		if (pt = PT.ptc_real) then tcode_real 
		else if (pt = PT.ptc_int32) then tcode_int32
		     else tcode_void
	   | (TC_VAR(i, j)) => RET[(VAR(vlookKE(kenv, i, j)))]
	   | (TC_TUPLE (_, [t1,t2])) =>
		(case (isFloat(kenv,t1), isFloat(kenv,t2))
		  of (YES, YES) => tcode_fpair
		   | ((NO, _) | (_, NO)) => tcode_pair
		   | ((MAYBE e, YES) | (YES, MAYBE e)) =>
			let val test = ieqLexp(e, tcode_real)
			 in COND(test, tcode_fpair, tcode_pair)
			end
		   | (MAYBE e1, MAYBE e2) =>
			let val e = iaddLexp(e1, e2)
			    val test = ieqLexp(e, tcode_realN 2)
			 in COND(test, tcode_fpair, tcode_pair)
			end)
	   | (TC_TUPLE (_, [])) => tcode_void
	   | (TC_TUPLE (_, ts)) => tcode_record
	   | (TC_ARROW (_,tc1,tc2)) => tcode_void
	   | (TC_ABS tx) => loop tx
	   | (TC_TOKEN(_,tx)) => loop tx           
	   | (TC_FIX((n,tx,ts), i)) => 
		let val ntx = 
                      (case ts 
                        of [] => tx
                         | _ => 
                            (case tc_out tx
                              of TC_FN(_, x) => x
                               | _ => bug "unexpected FIX 333 in tcLexp-loop"))
                    val tk = 
		     (case tc_out ntx
		       of TC_FN (ks, _) => List.nth(ks, i)
			| _ => bug "unexpected FIX tycs in tcLexp-loop")
		 in case tk_out tk
		     of TK_FUN(ks, _) => 
			  (let val (_, hdr) = tkTfn(kenv, ks)
			    in hdr(tcode_void)
			   end)
		      | _ => tcode_void
		end
	   | (TC_SUM _) => bug "unexpected TC_SUM tyc in tcLexp-loop"
	   | (TC_ENV _) => bug "unexpected TC_ENV tyc in tcLexp-loop"
	   | (TC_CONT _) => bug "unexpected TC_CONT tyc in tcLexp-loop"
	   | (TC_IND _) => bug "unexpected TC_IND tyc in tcLexp-loop"
	   | (TC_NVAR _) => bug "unexpected TC_NVAR tyc in tcLexp-loop"
	   |  _ => bug "unexpected tyc in tcLexp-loop")
   in loop tc
  end (* function tcLexp *)

and tcsLexp (kenv, ts) = 
  let fun h tc = tcLexp kenv tc
   in RECORDg(map h ts)
  end (* function tcsLexp *)

and tsLexp (kenv, ts) = 
  let fun h tc = tcLexp kenv tc
   in SRECORDg(map h ts)
  end (* function tsLexp *)

(** an improvement is to lift all of these code to the start of the 
    compilation unit *)
(*** THE FOLLOWING CODE IS ROUGH AND NEEDS TO BE POLISHED ! ***)
and isFloat (kenv, tc) = 
  let fun loop x = 
	(case (tc_out x)
	  of (TC_PRIM pt) => 
		if (pt = PT.ptc_real) then YES else NO
	   | (TC_TUPLE (_, ts)) => NO
	   | (TC_ARROW (_,tc1,tc2)) => NO
	   | (TC_TOKEN(_,tx)) => loop tx
	   | (TC_FIX(_, i)) => NO
	   | (TC_APP(tx, _)) => 
		(case tc_out tx
		  of (TC_APP _ | TC_PROJ _ | TC_VAR _) => 
		       MAYBE(tcLexp kenv x)
		   | _ => NO)
	  (* | (TC_ABS tx) => loop tx  *)
	   | (TC_VAR(i,j)) =>
		let val k = klookKE(kenv, i, j)
		 in case (tk_out k)
		     of TK_BOX => NO
		      | _ => MAYBE(tcLexp kenv x)
		end 
	   | _ => MAYBE(tcLexp kenv x))

   in loop tc
  end

fun isPair (kenv, tc) = 
  let fun loop x = 
	(case (tc_out x)
	  of (TC_PRIM pt) => NO
	   | (TC_TUPLE (_, [_,_])) => YES
	   | (TC_TUPLE _) => NO
	   | (TC_ARROW _) => NO
	   | (TC_TOKEN(_,tx)) => loop tx
	   | (TC_FIX(_, i)) => NO
	   | (TC_APP(tx, _)) => 
		(case tc_out tx
		  of (TC_APP _ | TC_PROJ _ | TC_VAR _ | TC_NVAR _) => 
		       MAYBE(tcLexp kenv x)
		   | _ => NO)
       (*    | (TC_ABS tx) =>  loop tx  *)
	   | _ => MAYBE(tcLexp kenv x))

   in loop tc
  end

(****************************************************************************
 *                      TYPED INTERPRETATION OF UNTAGGED                    *
 ****************************************************************************)
(** tc is of kind Omega; this function tests whether tc can be int31 ? *)
fun tcTag (kenv, tc) = 
  let fun loop x =     (* a lot of approximations in this function *)
	(case (tc_out x)
	  of (TC_PRIM pt) => if PT.unboxed pt then NO else YES
                (* if PT.ubxupd pt then YES else NO *)
		    (* this is just an approximation *)
	   | (TC_TUPLE (_, [])) => YES
	   | (TC_TUPLE (_, ts)) => NO
	   | (TC_ARROW (_,tc1,tc2)) => YES (* NO *)
	   | (TC_ABS tx) => loop tx
	   | (TC_TOKEN(_,tx)) => loop tx
	   | (TC_FIX(_, i)) => YES
	   | (TC_APP(tx, _)) => 
		(case tc_out tx
		  of (TC_APP _ | TC_PROJ _ | TC_VAR _) => 
		       MAYBE (tcLexp kenv x)
		   | _ => YES)
	   | _ => (MAYBE (tcLexp kenv x)))
   in loop tc
  end (* function tcTag *)

(* val utgc : tyc * kenv * tyc -> value -> lexp *)
fun utgc (tc, kenv, rt) = 
  (case tcTag(kenv, tc)
    of YES => (fn u => let val v = mkv()
                        in RECORD(FU_rk_tuple, [u], v, 
                             WRAP(LT.tcc_tuple[rt], VAR v))
                       end)
     | NO => (fn u => WRAP(rt, u))
     | MAYBE ne => 
	 (fn u => let val v = mkv()
                      val hh = ieqLexp(ne, tcode_void)
                   in COND(hh, RECORD(FU_rk_tuple, [u], v, 
                                      WRAP(LT.tcc_tuple[rt], VAR v)),
                               WRAP(rt, u))                           
           	  end))
	    
(* val utgd : tyc * kenv * tyc -> value -> lexp *) 
fun utgd (tc, kenv, rt) = 
  (case tcTag(kenv, tc)
    of YES => (fn u => let val v = mkv() and z = mkv()
                        in FU_UNWRAP(LT.tcc_tuple [rt], [u], v, 
                               SELECT(VAR v, 0, z, RET[VAR z]))
                       end)
     | NO => (fn u => UNWRAP(rt, u))
     | MAYBE ne => 
          (fn u => let val v = mkv() and z = mkv()
                       val hh = ieqLexp(ne, tcode_void)
                    in COND(hh, FU_UNWRAP(LT.tcc_tuple [rt], [u], v, 
                               SELECT(VAR v, 0, z, RET[VAR z])),
                            UNWRAP(rt, u))
                   end))

(* val tgdc : int * tyc * kenv * tyc -> value -> lexp *)
fun tgdc (i, tc, kenv, rt) = 
  let val nt = LT.tcc_tuple [LT.tcc_int, rt]
   in fn u => let val x = mkv()
               in RECORD(FU_rk_tuple, [INT i, u], x, WRAP(nt, VAR x))
              end
  end

(* val tgdd : int * tyc * kenv * tyc -> value -> lexp *)
fun tgdd (i, tc, kenv, rt) = 
  let val nt = LT.tcc_tuple [LT.tcc_int, rt]
   in fn u => let val x = mkv() and v = mkv()
               in FU_UNWRAP(nt, [u], x, SELECT(VAR x, 1, v, RET[VAR v]))
              end
  end

(****************************************************************************
 *                      TYPED INTERPRETATION OF FP RECORD                   *
 ****************************************************************************)
(** tc is a ground tyc of kind Omega, only record types and arrow types are 
    interesting for the time being. *)
(** all of these wrappers probably should be lifted to the top of the
    program, otherwise we may run into space blow-up ! *)
(* val tcCoerce : kenv * tyc * bool * bool -> (lexp -> lexp) option *)
fun tcCoerce (kenv, tc, nt, wflag, b) = 
  (case (tc_out tc, tc_out nt)
    of (TC_TUPLE (_, ts), _) =>
	 let fun h([], i, e, el, 0) = NONE
	       | h([], i, e, el, res) = 
		   let val w = mkv()                 
		       val wx = VAR w
		       fun g(i, NONE) =  SELECTv(i, wx)
			 | g(i, SOME _) = 
			     if wflag then 
			       UNWRAPg(LT.tcc_real, b, SELECTv(i, wx))
			     else WRAPg(LT.tcc_real, b, SELECTv(i, wx))

		       val ntc = LT.tcc_tuple(map (fn _ => LT.tcc_real) ts)

		       val ne = RECORDg (map g (rev el))
		       val test = ieqLexp(e, tcode_realN res) 

		       fun hdr0 xe = 
			 if wflag then 
			   COND(test, LET([w], xe, WRAPcast(ntc, b, ne)), 
				      WRAPcast(nt, b, xe))
			 else COND(test, LET([w], UNWRAPcast(ntc, b, xe), ne), 
					 UNWRAPcast(nt, b, xe))

		       fun hdr (xe as (RET[(VAR _)])) = hdr0 xe
			 | hdr xe = let val z = mkv()
				     in LET([z], xe, hdr0 (RET[VAR z]))
				    end
		    in SOME hdr
		   end
	       | h(a::r, i, e, el, res) = 
		   (case isFloat(kenv, a) 
		     of NO => NONE
		      | YES => h(r, i+1, e, (i,NONE)::el, res)
		      | MAYBE z => h(r, i+1, iaddLexp(e, z), 
				     (i, SOME a)::el, res+1))

	  in h(ts, 0, RET[INT 0], [], 0)
	 end
     | (TC_ARROW _, _) => (* (tc1, tc2) => *)
        let val (tc1, _) = LT.tcd_parrow tc
            val (_, tc2) = LT.tcd_parrow nt
         in (case isPair(kenv, tc1)
              of (YES | NO) => NONE
               | (MAYBE e) =>
                 let val w = mkv()
                     val test1 = ieqLexp(RET[(VAR w)], tcode_pair)
                     val test2 = ieqLexp(RET[(VAR w)], tcode_fpair)
                     val m = mkv() and m2 = mkv()
                     val n = mkv() and n2 = mkv()

                     val tc_real = LT.tcc_real
                     val tc_breal = LT.tcc_void (* LT.tcc_wrap tc_real *)
                     val lt_breal = LT.ltc_tyc tc_breal
                     val tc_void = LT.tcc_void
                     val lt_void = LT.ltc_void
                     val tc_pair = LT.tcc_tuple [tc_void, tc_void]
                     val tc_fpair = LT.tcc_tuple [tc_real, tc_real]
                     val tc_bfpair = LT.tcc_tuple [tc_breal, tc_breal]
                     val lt_pair = LT.ltc_tyc tc_pair
                     val lt_fpair = LT.ltc_tyc tc_fpair
                     val lt_bfpair = LT.ltc_tyc tc_bfpair
                     val ident = fn le => le

                     val (argt1, body1, hh1) = 
                       if wflag then (* wrapping *)
                         ([(m,lt_void),(m2,lt_void)], 
                          fn sv => 
                            let val xx = mkv() and yy = mkv()
                             in RECORD(FU_rk_tuple, [VAR m, VAR m2], xx,
                                  FU_WRAP(tc_pair, [VAR xx], yy,
                                    APP(sv, [VAR yy])))
                            end,
                          fn le => 
                            WRAPcast(mkarw([tc_void,tc_void],[tc2]), 
                                     true, le))
                       else (* unwrapping *)
                         let val x = mkv() and y = mkv() and z = mkv()
                          in ([(m, lt_void)], 
                              fn sv => 
                                let val xx = mkv() 
                                 in LET([xx], 
                                      UNWRAPcast(
                                         mkarw([tc_void, tc_void], [tc2]),
                                              true, RET[sv]),
                                        FU_UNWRAP(tc_pair, [VAR m], x, 
                                         SELECT(VAR x, 0, y, 
                                         SELECT(VAR x, 1, z, 
                                          APP(VAR xx, [VAR y, VAR z])))))
                                end,
                             ident)
                         end

                     val (argt2, body2, hh2) = 
                       if wflag then  (* wrapping *)
                         ([(n,lt_breal),(n2,lt_breal)],
                          fn sv =>
                            let val xx = mkv() and yy = mkv()
                             in LET ([xx], 
                                   RECORDg [UNWRAP(tc_real, VAR n),
                                            UNWRAP(tc_real, VAR n2)],
                                FU_WRAP(tc_fpair, [VAR xx], yy,
                                   APP(sv, [VAR yy])))
                            end,
                          fn le => WRAPcast(mkarw([tc_breal,tc_breal],[tc2]), 
                                            true, le))
                       else  (* unwrapping *)
                         let val x = mkv() and y = mkv() and z = mkv()
                             val q0 = mkv() and q1 = mkv()
                          in ([(n, lt_void)],
                              fn sv => 
                                let val xx = mkv()
                                 in LET([xx],
                                      UNWRAPcast(
                                         mkarw([tc_breal, tc_breal], [tc2]),
                                            true, RET[sv]),
                                      FU_UNWRAP(tc_fpair, [VAR n], x, 
                                        SELECT(VAR x, 0, y, 
                                          FU_WRAP(tc_real, [VAR y], q0,
                                        SELECT(VAR x, 1, z, 
                                          FU_WRAP(tc_real, [VAR z], q1,
                                         APP(VAR xx, [VAR q0, VAR q1])))))))
                                end,
                            ident)
                         end

                     val hh3 = if wflag then fn le => WRAPcast(nt, true, le)
                               else fn le => UNWRAPcast(nt, true, le)

                     (*** NEEDS MORE WORK TO DO THE RIGHT COERCIONS ***)
                     fun hdr0(sv) =
                       LET([w], e, 
                         COND(test1, hh1(FNg(argt1, body1 sv)),
                           COND(test2, hh2(FNg(argt2, body2 sv)),
                                hh3(RET[sv]))))

                     fun hdr (xe as RET [sv]) = hdr0 sv
                       | hdr xe = let val z = mkv()
                                   in LET([z], xe, hdr0(VAR z))
                                  end
                  in SOME hdr
                 end)
        end
     | _ => NONE)

(* val mkwrp : tyc * kenv * bool * tyc -> lexp -> lexp *)
fun mkwrp (tc, kenv, b, nt) = 
  (case tcCoerce(kenv, tc, nt, true, b)
    of NONE => (fn le => WRAPg(nt, b, le))
     | SOME hdr => hdr)

(* val mkuwp  : tyc * kenv * bool * tyc -> lexp -> lexp *)
fun mkuwp (tc, kenv, b, nt) = 
  (case tcCoerce(kenv, tc, nt, false, b)
    of NONE => (fn le => UNWRAPg(nt, b, le))
     | SOME hdr => hdr)

val realSub = PO.NUMSUBSCRIPT{kind=PO.FLOAT 64, checked=false, immutable=false}
val realUpd = PO.NUMUPDATE{kind=PO.FLOAT 64, checked=false}

fun rsubLexp (vs, t) = 
  let val x = mkv()
   in PRIMOP((NONE, realSub, t, []), vs, x, RET[VAR x])
  end

fun rupdLexp (vs, t) = 
  let val x = mkv()
   in PRIMOP((NONE, realUpd, t, []), vs, x, RET[VAR x])
  end

fun subLexp (vs, t) = 
  let val x = mkv()
   in PRIMOP((NONE, PO.SUBSCRIPT, t, []), vs, x, RET[VAR x])
  end

fun updLexp (po, vs, t) = 
  let val x = mkv()
   in PRIMOP((NONE, po, t, []), vs, x, RET[VAR x])
  end


fun arrSub (tc, kenv, blt, rlt) = 
  let val nt = blt
      val rnt = rlt
   in (case isFloat(kenv, tc)
        of NO => (fn vs => subLexp(vs, nt))
         | YES => (fn vs => WRAPg(LT.tcc_real, true, rsubLexp(vs, rnt)))
         | MAYBE z =>
             (let val test = ieqLexp(z, tcode_real)
               in (fn vs =>
                     COND(test, WRAPg(LT.tcc_real, true, rsubLexp(vs, rnt)),
                          subLexp(vs, nt)))
              end))
  end

fun arrUpd(tc, kenv, po, blt, rlt) = 
  let val nt = blt
      val rnt = rlt
   in (case isFloat(kenv,tc)
        of NO => (fn vs => updLexp(po, vs, nt))
         | YES => (fn [x,y,z] => 
                     let val nz = mkv()
                      in LET([nz], UNWRAPg(LT.tcc_real, true, RET[z]),
                             rupdLexp([x,y,VAR nz], rnt))
                     end)
         | MAYBE z => 
             (let val test = ieqLexp(z, tcode_real)
               in (fn (vs as [x,y,z]) => 
                     COND(test, 
                          let val nz = mkv()
                           in LET([nz], UNWRAPg(LT.tcc_real, true, RET[z]),
                                  rupdLexp([x,y,VAR nz], rnt))
                          end,
                          updLexp(po, vs, nt)))
              end))
  end

fun arrNew(tc, pv, rv, kenv) = 
  (case isFloat(kenv,tc)
    of NO => (fn vs => 
                let val x= mkv()
                 in LET([x], APPg(RET[VAR pv], tsLexp(kenv, [tc])),
                        APP(VAR x, vs))
                end) 
     | YES => (fn (vs as [x,y]) => 
                let val z = mkv()
                 in LET([z], UNWRAPg(LT.tcc_real, true, RET[y]),
                        APP(VAR rv, [x, VAR z]))
                end)
     | MAYBE z => 
         (let val test = ieqLexp(z, tcode_real)
           in (fn (vs as [x,y]) =>
                 COND(test, 
                      let val z = mkv()
                       in LET([z], UNWRAPg(LT.tcc_real, true, RET[y]),
                              APP(VAR rv, [x, VAR z]))
                      end,
                      let val z= mkv()
                       in LET([z], APPg(RET[VAR pv], tsLexp(kenv, [tc])),
                          APP(VAR z, vs))
                      end))
          end))

end (* toplevel local *)
end (* structure TypeOper *)



(*
 * $Log$
 *)
