(* Copyright 1996 by Bell Laboratories *)
(* typeoper.sml *)

signature TYPEOPER = 
sig
  type kenv
  val initKE : kenv

  val tkLexp : kenv * LtyKernel.tkind list -> 
                              (kenv * (Lambda.lexp -> Lambda.lexp))

  val tcLexp  : kenv * LtyKernel.tyc -> Lambda.lexp
  val tsLexp : kenv * LtyKernel.tyc list -> Lambda.lexp 

  val utgc : kenv * LtyKernel.tyc -> Lambda.lexp -> Lambda.lexp 
  val utgd : kenv * LtyKernel.tyc -> Lambda.lexp -> Lambda.lexp 
  val tgdc : kenv * int * LtyKernel.tyc -> Lambda.lexp -> Lambda.lexp 
  val tgdd : kenv * int * LtyKernel.tyc -> Lambda.lexp -> Lambda.lexp 

  val mkwrp : kenv * bool * LtyKernel.tyc -> Lambda.lexp -> Lambda.lexp 
  val mkuwp : kenv * bool * LtyKernel.tyc -> Lambda.lexp -> Lambda.lexp 

  val arrSub : kenv * LtyKernel.lty * LtyKernel.tyc 
                 -> Lambda.value -> Lambda.lexp
  val arrUpd : kenv * LtyKernel.lty * LtyKernel.tyc 
                 -> Lambda.value -> Lambda.lexp
  val arrNew : kenv * LtyKernel.lty * LtyKernel.tyc * LambdaVar.lvar 
                 * LambdaVar.lvar -> Lambda.value -> Lambda.lexp

end (* signature TYPEOPER *)

structure TypeOper : TYPEOPER = 
struct

local structure DI = DebIndex
      structure LT = LtyExtern
      structure LU = LtyUtil
      structure LV = LambdaVar
      structure PO = PrimOp
      structure PT = PrimTyc
      structure BT = BasicTypes 
      structure TP = Types
      open LtyKernel Lambda
in

(****************************************************************************
 *                  UTILITY FUNCTIONS AND CONSTANTS                         * 
 ****************************************************************************) 

fun bug s = ErrorMsg.impossible ("LtyPrim: " ^ s)
fun say (s : string) = Control.Print.say s

val mkv = LV.mkLvar
val ident = fn le => le

fun split(SVAL v) = (v, ident)
  | split x = let val v = mkv()
               in (VAR v, fn z => LET(v, x, z))
              end

fun ltAppSt (lt, ts) = 
  (case LT.lt_inst(lt, ts) 
    of [b] => b 
     | _ => bug "unexpected case in ltAppSt")

fun SELECTg(i, e) = 
  let val (v, hdr) = split e
   in hdr(SELECT(i, v))
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

fun WRAPcast (z, b, e) = 
  let val (v, h) = split e
      val pt = LT.ltc_arw(LT.ltc_tyc z, LT.ltc_tyc(LT.tcc_box z))
      val pv = PRIM(PO.CAST,pt,[])
   in h(APP(pv, v))
  end

fun UNWRAPcast (z, b, e) = 
  let val (v, h) = split e
      val pt = LT.ltc_arw(LT.ltc_tyc(LT.tcc_box z), LT.ltc_tyc z)
      val pv = PRIM(PO.CAST,pt,[])
   in h(APP(pv, v))
  end

fun SWITCHg (e, s, ce, d) = 
  let val (v, h) = split e
   in h(SWITCH(v, s, ce, d))
  end

fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []

fun option(NONE) = false
  | option(SOME _) = true

fun exists(p, a::r) = if p a then true else exists(p, r)
  | exists(p, []) = false

fun opList l = exists(option, l)

fun force (NONE, le) = le
  | force (SOME f, le) = f le

val boolsign = BT.boolsign
val (trueDcon', falseDcon') = 
  let val lt = LT.ltc_parrow(LT.ltc_unit, LT.ltc_bool)
      fun h (TP.DATACON{name,rep,typ,...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

fun COND(a,b,c) =
  SWITCHg(a, boolsign, [(DATAcon(trueDcon'),b),
		       (DATAcon(falseDcon'),c)], NONE)

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

val tkLty = LU.tkLty
 
(* val tkLexp: kenv * tkind list -> kenv * (lexp -> lexp) *)
fun tkLexpG (kenv, ks, record) = 
  let val w = mkv()
      val vs = map (fn _ => mkv ()) ks
      val argt = record(map tkLty ks)
      fun h([], i, base) = base
	| h(v::r, i, base) = h(r, i+1, LET(v, SELECT(i, VAR w), base))
      fun hdr le = FN(w, argt, h(vs, 0, le))
   in (addKE(kenv, vs, ks), hdr)
  end

fun tkLexp (kenv, ks) = tkLexpG(kenv, ks, LT.ltc_str)

(** mapping TC_VAR to proper lvars; TC_PRIM to proper constants *)
(** the actual type calculations should be lifted up till the innermost TFN *)
(* val tcLexp : kenv * tyc -> lexp *)

val tcode_void = SVAL(INT 0)
val tcode_record = SVAL(INT 1)
val tcode_int32 = SVAL(INT 2)
val tcode_pair = SVAL(INT 3)
val tcode_fpair = SVAL(INT 4)
val tcode_real = SVAL(INT 5)
fun tcode_realN n = SVAL(INT(n * 5))

datatype outcome 
  = YES
  | NO
  | MAYBE of lexp  

val intty = LT.ltc_int
val boolty = LT.ltc_bool
val inteqty = LT.ltc_arw(LT.ltc_tuple [intty, intty], boolty)
val intopty = LT.ltc_arw(LT.ltc_tuple [intty, intty], intty)
val ieq = SVAL(PRIM(PO.IEQL, inteqty, []))
val iadd = SVAL(PRIM(PO.IADD, intopty, []))

fun tcLexp (kenv, tc) = 
  let fun loop x = 
	(case (tc_out x)
	  of (TC_FN(ks, tx)) => 
		let val (nenv, hdr) = tkLexpG(kenv, ks, LT.ltc_tuple)
		 in hdr(tcLexp(nenv, tx))
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
	   | (TC_VAR(i, j)) => SVAL(VAR(vlookKE(kenv, i, j)))
	   | (TC_TUPLE (_, [t1,t2])) =>
		(case (isFloat(kenv,t1), isFloat(kenv,t2))
		  of (YES, YES) => tcode_fpair
		   | ((NO, _) | (_, NO)) => tcode_pair
		   | ((MAYBE e, YES) | (YES, MAYBE e)) =>
			let val test = APPg(ieq, RECORDg[e, tcode_real])
			 in COND(test, tcode_fpair, tcode_pair)
			end
		   | (MAYBE e1, MAYBE e2) =>
			let val e = APPg(iadd, RECORDg [e1, e2])
			    val test = APPg(ieq, RECORDg [e, tcode_realN 2])
			 in COND(test, tcode_fpair, tcode_pair)
			end)
	   | (TC_TUPLE (_, ts)) => tcode_record
	   | (TC_ARROW (_,tc1,tc2)) => tcode_void
	   | (TC_ABS tx) => loop tx
	   | (TC_BOX tx) => loop tx           
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
			  (let val (_, hdr) = 
					tkLexpG(kenv, ks, LT.ltc_tuple)
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
  let fun h tc = tcLexp(kenv, tc)
   in RECORDg(map h ts)
  end (* function tcsLexp *)

and tsLexp (kenv, ts) = 
  let fun h tc = tcLexp(kenv, tc)
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
	   | (TC_BOX tx) => NO     (* this requires further thoughts ! *)
	   | (TC_FIX(_, i)) => NO
	   | (TC_APP(tx, _)) => 
		(case tc_out tx
		  of (TC_APP _ | TC_PROJ _ | TC_VAR _) => 
		       MAYBE(tcLexp(kenv, x))
		   | _ => NO)
	  (* | (TC_ABS tx) => loop tx  *)
	   | (TC_VAR(i,j)) =>
		let val k = klookKE(kenv, i, j)
		 in case (tk_out k)
		     of TK_BOX => NO
		      | _ => MAYBE(tcLexp(kenv, x))
		end 
	   | _ => MAYBE(tcLexp(kenv, x)))

   in loop tc
  end

fun isPair (kenv, tc) = 
  let fun loop x = 
	(case (tc_out x)
	  of (TC_PRIM pt) => NO
	   | (TC_TUPLE (_, [_,_])) => YES
	   | (TC_TUPLE _) => NO
	   | (TC_ARROW _) => NO
	   | (TC_BOX tx) => NO     (* this requires further thoughts !!! *)
	   | (TC_FIX(_, i)) => NO
	   | (TC_APP(tx, _)) => 
		(case tc_out tx
		  of (TC_APP _ | TC_PROJ _ | TC_VAR _ | TC_NVAR _) => 
		       MAYBE(tcLexp(kenv, x))
		   | _ => NO)
       (*    | (TC_ABS tx) =>  loop tx  *)
	   | _ => MAYBE(tcLexp(kenv, x)))

   in loop tc
  end

(****************************************************************************
 *                      TYPED INTERPRETATION OF UNTAGGED                    *
 ****************************************************************************)
(** tc is of kind Omega; this function tests whether tc can be int31 ? *)
fun tcTag (kenv, tc) = 
  let fun loop x = 
	(case (tc_out x)
	  of (TC_PRIM pt) => if PT.unboxed pt then NO else YES
		    (* this is just an approximation *)
	   | (TC_TUPLE (_, ts)) => NO
	   | (TC_ARROW (_,tc1,tc2)) => YES
	   | (TC_ABS tx) => loop tx
	   | (TC_BOX tx) => loop tx
	   | (TC_FIX(_, i)) => YES
	   | (TC_APP(tx, _)) => 
		(case tc_out tx
		  of (TC_APP _ | TC_PROJ _ | TC_VAR _) => 
		       (let val e1 = tcLexp(kenv, x)
			 in MAYBE(APPg(ieq, RECORDg[e1, tcode_void]))
			end) 
		   | _ => YES)
	   | _ => (let val e1 = tcLexp(kenv, x)
		    in MAYBE(APPg(ieq, RECORDg[e1, tcode_void]))
		   end))
   in loop tc
  end (* function tcTag *)

(* val utgc : kenv * tyc -> lexp -> lexp *)
fun utgc (kenv, tc) = 
  (case tcTag(kenv, tc)
    of YES => (fn le => WRAPg(LT.tcc_tuple [tc], true, RECORDg[le]))
     | NO => (fn le => le)
     | MAYBE ne => 
	let fun h(x as (SVAL(VAR v))) =  
		  COND(ne, WRAPg(LT.tcc_tuple [tc], true, RECORDg [x]),
			   x)
	      | h x = 
		  let val w = mkv() 
		   in LET(w, x, 
		       COND(ne, WRAPg(LT.tcc_tuple [tc], true, RECORD [VAR w]),
				SVAL(VAR w)))
		  end
	 in h
	end)
	    
(* val utgd : kenv * tyc -> lexp -> lexp *)
fun utgd (kenv, tc) = 
  (case tcTag(kenv, tc)
    of YES => 
	(fn le => SELECTg(0, UNWRAPg(LT.tcc_tuple [tc], true, le)))
     | NO => (fn le => le)
     | MAYBE ne => 
	let fun h(x as (SVAL(VAR v))) =  
		  COND(ne, SELECTg(0, UNWRAPg(LT.tcc_tuple [tc], true, x)), x)
	      | h x = 
		  let val w = mkv()
		   in LET(w, x, 
		       COND(ne, SELECTg(0, UNWRAP(LT.tcc_tuple [tc],true,VAR w)),
				SVAL(VAR w)))
		  end
	 in h
	end)

(* val tgdc : kenv * int * tyc -> lexp -> lexp *)
fun tgdc (kenv, i, tc) = 
  let val nt = LT.tcc_tuple [LT.tcc_int, tc]
   in (fn le => WRAPg(nt, true, RECORDg [SVAL(INT i), le]))
  end

(* val tgdd : kenv * int * tyc -> lexp -> lexp *)
fun tgdd (kenv, i, tc) = 
  let val nt = LT.tcc_tuple [LT.tcc_int, tc]
   in (fn le => SELECTg(1, UNWRAPg(nt, true, le)))
  end


(****************************************************************************
 *                      TYPED INTERPRETATION OF FP RECORD                   *
 ****************************************************************************)
(** tc is a ground tyc of kind Omega, only record types and arrow types are 
    interesting for the time being. *)
(** all of these wrappers probably should be lifted to the top of the
    program, otherwise we may run into space blow-up ! *)
(* val tcCoerce : kenv * tyc * bool * bool -> (lexp -> lexp) option *)
fun tcCoerce (kenv, tc, wflag, b) = 
  (case tc_out tc
    of TC_TUPLE (_, ts) =>
	 let fun h([], i, e, el, 0) = NONE
	       | h([], i, e, el, res) = 
		   let val w = mkv()                 
		       val wx = VAR w
		       fun g(i, NONE) =  SELECT(i, wx)
			 | g(i, SOME _) = 
			     if wflag then 
			       UNWRAPg(LT.tcc_real, b, SELECT(i, wx))
			     else WRAPg(LT.tcc_real, b, SELECT(i, wx))

		       val ntc = LT.tcc_tuple(map (fn _ => LT.tcc_real) ts)

		       val ne = RECORDg (map g (rev el))
		       val test = APPg(ieq, RECORDg[e, tcode_realN res]) 

		       fun hdr0 xe = 
			 if wflag then 
			   COND(test, LET(w, xe, WRAPcast(ntc, b, ne)), 
				      WRAPcast(tc, b, xe))
			 else COND(test, LET(w, UNWRAPcast(ntc, b, xe), ne), 
					 UNWRAPcast(tc, b, xe))

		       fun hdr (xe as (SVAL(VAR _))) = hdr0 xe
			 | hdr xe = let val z = mkv()
				     in LET(z, xe, hdr0 (SVAL(VAR z)))
				    end
		    in SOME hdr
		   end
	       | h(a::r, i, e, el, res) = 
		   (case isFloat(kenv, a) 
		     of NO => NONE
		      | YES => h(r, i+1, e, (i,NONE)::el, res)
		      | MAYBE z => h(r, i+1, APPg(iadd, RECORDg [e, z]), 
				     (i, SOME a)::el, res+1))

	  in h(ts, 0, SVAL(INT 0), [], 0)
	 end
     | TC_ARROW _ => (* (tc1, tc2) => *)
        let val (tc1, tc2) = LT.tcd_parrow tc
         in (case isPair(kenv, tc1)
              of (YES | NO) => NONE
               | (MAYBE e) =>
                 let val w = mkv()
                     val test1 = APPg(ieq, RECORDg[SVAL(VAR w), tcode_pair])
                     val test2 = APPg(ieq, RECORDg[SVAL(VAR w), tcode_fpair])
                     val m = mkv()
                     val n = mkv()

                     val tc_real = LT.tcc_real
                     val tc_breal = LT.tcc_box tc_real
                     val tc_void = LT.tcc_void
                     val lt_void = LT.ltc_void
                     val tc_pair = LT.tcc_tuple [tc_void, tc_void]
                     val tc_fpair = LT.tcc_tuple [tc_real, tc_real]
                     val tc_bfpair = LT.tcc_tuple [tc_breal, tc_breal]
                     val lt_pair = LT.ltc_tyc tc_pair
                     val lt_fpair = LT.ltc_tyc tc_fpair
                     val lt_bfpair = LT.ltc_tyc tc_bfpair
                     val ident = fn le => le

                     val (argt1, body1, hh1, ih1) = 
                       if wflag then (* wrapping *)
                         (lt_pair, WRAP(tc_pair, true, VAR m),
                          fn le => WRAPcast(LT.tcc_parrow(tc_pair,tc2), true, le),
                          ident)
                       else (* unwrapping *)
                         let val q = mkv()
                          in (lt_void, UNWRAP(tc_pair, true, VAR m),ident,
                              fn le => UNWRAPcast(LT.tcc_parrow(tc_pair, tc2),
                                               true, le))
                         end

                     val (argt2, body2, hh2, ih2) = 
                       if wflag then
                         (lt_bfpair, WRAPg(tc_fpair, true, 
                           RECORDg [UNWRAPg(tc_real, true, SELECT(0, VAR n)),
                                    UNWRAPg(tc_real, true, SELECT(1, VAR n))]),
                          fn le => WRAPcast(LT.tcc_parrow(tc_bfpair,tc2), true, le),
                          ident)
                       else
                         let val q = mkv()
                          in (lt_void, LET(q, UNWRAP(tc_fpair, true, VAR n),
                            RECORDg [WRAPg(tc_real, true, SELECT(0, VAR q)),
                                     WRAPg(tc_real, true, SELECT(1, VAR q))]),
                            ident,
                            fn le => UNWRAPcast(LT.tcc_parrow(tc_bfpair, tc2),
                                             true, le))
                         end

                     val hh3 = if wflag then fn le => WRAPcast(tc, true, le)
                               else fn le => UNWRAPcast(tc, true, le)

                     (*** NEEDS MORE WORK TO DO THE RIGHT COERCIONS ***)
                     fun hdr0(sv) =
                       LET(w, e, 
                         COND(test1, hh1(FN(m, argt1, 
                                       APPg(ih1(SVAL sv), body1))),
                           COND(test2, hh2(FN(n, argt2, 
                                       APPg(ih2(SVAL sv), body2))),
                                hh3(SVAL sv))))

                     fun hdr (xe as SVAL sv) = hdr0 sv
                       | hdr xe = let val z = mkv()
                                   in LET(z, xe, hdr0(VAR z))
                                  end
                  in SOME hdr
                 end)
        end
     | _ => NONE)

(* val mkwrp : kenv * bool * tyc -> lexp -> lexp *)
fun mkwrp (kenv, b, tc) = 
  (case tcCoerce(kenv, tc, true, b)
    of NONE => (fn le => WRAPg(tc, b, le))
     | SOME hdr => hdr)

(* val mkuwp : kenv * bool * tyc -> lexp -> lexp *)
fun mkuwp (kenv, b, tc) = 
  (case tcCoerce(kenv, tc, false, b)
    of NONE => (fn le => UNWRAPg(tc, b, le))
     | SOME hdr => hdr)

val realSub = PO.NUMSUBSCRIPT{kind=PO.FLOAT 64, checked=false, immutable=false}
val realUpd = PO.NUMUPDATE{kind=PO.FLOAT 64, checked=false}

fun arrSub(kenv, lt, tc) = 
  let val nt = LT.lt_pinst_st(lt, [tc])
      val rnt = LT.lt_pinst_st(lt, [LT.tcc_real])
   in (case isFloat(kenv, tc)
        of NO => (fn sv => APP(PRIM(PO.SUBSCRIPT, nt, []), sv))
         | YES => (fn sv => WRAPg(LT.tcc_real, true, 
                                  APP(PRIM(realSub, rnt, []), sv)))
         | MAYBE z =>
             (let val test = APPg(ieq, RECORDg[z, tcode_real])
               in (fn sv =>
                     COND(test, WRAPg(LT.tcc_real, true, 
                                  APP(PRIM(realSub, rnt, []), sv)),
                          APP(PRIM(PO.SUBSCRIPT, nt, []), sv)))
              end))
  end

fun arrUpd(kenv, lt, tc) = 
  let val nt = LT.lt_pinst_st(lt, [tc])
      val rnt = LT.lt_pinst_st(lt, [LT.tcc_real])
   in (case isFloat(kenv,tc)
        of NO => (fn sv => APP(PRIM(PO.UPDATE, nt, []), sv))
         | YES => (fn sv => APPg(SVAL(PRIM(realUpd, rnt, [])), 
                              RECORDg[SELECT(0, sv),
                                      SELECT(1, sv),
                                 UNWRAPg(LT.tcc_real, true, 
                                         SELECT(2, sv))]))
         | MAYBE z => 
             (let val test = APPg(ieq, RECORDg[z, tcode_real])
               in (fn sv => 
                     COND(test, APPg(SVAL(PRIM(realUpd, rnt, [])), 
                              RECORDg[SELECT(0, sv),
                                      SELECT(1, sv),
                                 UNWRAPg(LT.tcc_real, true, 
                                         SELECT(2, sv))]),
                          APP(PRIM(PO.UPDATE, nt, []), sv)))
              end))
  end

fun arrNew(kenv, lt, tc, pv, rv) = 
  (case isFloat(kenv,tc)
    of NO => (fn sv => APPg(APPg(SVAL(VAR pv), tsLexp(kenv, [tc])), SVAL sv))
     | YES => (fn sv => APPg(SVAL(VAR rv), 
                   RECORDg [SELECT(0, sv), 
                            UNWRAPg(LT.tcc_real, true, SELECT(1, sv))]))
     | MAYBE z => 
         (let val test = APPg(ieq, RECORDg[z, tcode_real])
           in (fn sv => 
                 COND(test, APPg(SVAL(VAR rv), 
                   RECORDg [SELECT(0, sv), 
                            UNWRAPg(LT.tcc_real, true, SELECT(1, sv))]),
                    APPg(APPg(SVAL(VAR pv), tsLexp(kenv, [tc])), SVAL sv)))
          end))

end (* toplevel local *)
end (* structure TypeOper *)


(*
 * $Log: ltyprim.sml,v $
 * Revision 1.5  1998/01/07 15:18:16  dbm
 *   Fixing bug 1323. Wrapping and unwrapping primitives were usually ignored
 *   in the cpstrans phase before we perform the cps optimization. Unfortunately,
 *   they could lead to ill-typed CPS programs. To resolve this, I turn those
 *   sensitive wrap and unwrap primitives into "casts"; I leave the casts in the
 *   code; the cps generic phase will generate a move for each cast. In the
 *   long term, we have to think thoroughly about the meanings of these wrapping
 *   primitives and how they interface with compile-time optimizations.
 *
 * Revision 1.4  1997/05/05 20:00:13  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.3  1997/04/18  15:49:02  george
 *   Cosmetic changes on some constructor names. Changed the shape for
 *   FIX type to potentially support shared dtsig. -- zsh
 *
 * Revision 1.2  1997/02/26  21:53:57  george
 *    Fixing the incorrect wrapper bug, BUG 1158, reported by Ken Cline
 *    (zcline.sml). This also fixes the core dump bug, BUG 1153,
 *    reported by Nikolaj.
 *
 *)
