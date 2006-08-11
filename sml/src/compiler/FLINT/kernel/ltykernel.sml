(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ltykernel.sml *)

structure LtyKernel :> LTYKERNEL = 
struct 

structure PT = PrimTyc
structure DI = DebIndex
open Lty

val debugging : bool ref = ref false
val dp : int ref = ref 20
fun bug s = ErrorMsg.impossible ("LtyKernel:" ^ s)

structure PP = PrettyPrintNew
structure PU = PPUtilNew
structure EM = ErrorMsg
open PPLty

val with_pp = PP.with_default_pp 

fun dgPrint (msg: string, printfn: PP.stream -> 'a -> unit, arg: 'a) =
  if (!debugging)
  then with_pp
	(fn ppstrm =>
	  (PP.openHVBox ppstrm (PP.Rel 0);
	   PP.string ppstrm msg;
	   PP.newline ppstrm;
	   PP.nbSpace ppstrm 2;
	   PP.openHVBox ppstrm (PP.Rel 0);
	   printfn ppstrm arg;
	   PP.closeBox ppstrm;
	   PP.newline ppstrm;
	   PP.closeBox ppstrm;
	   PP.flushStream ppstrm))
  else ()

(** utility functions for tc_env and lt_env *)
local

  fun tcc_env_int(x, 0, 0, te) = x
    | tcc_env_int(x, i, j, te) = tc_injX(TC_ENV(x, i, j, te))

  fun ltc_env_int(x, 0, 0, te) = x
    | ltc_env_int(x, i, j, te) = lt_injX(LT_ENV(x, i, j, te))
 
  fun withEff ([], ol, nl, tenv) = false
    | withEff (a::r, ol, nl, tenv) = 
        let val (i, j) = tvDecode a
            val neweff = 
              if i > ol then (ol <> nl)
              else (* case tcLookup(i, tenv)
                       of (NONE, n) => (nl - n) <> i
                        | (SOME ts, n) =>
                             (let val y = List.nth(ts, j)
                               in (case tc_outX y
                                    of TC_VAR(ni, nj) =>
                                        ((nj <> j) orelse ((ni+nl-n) <> i))
                                     | _ => true)
                              end) *) true
         in neweff orelse (withEff(r, ol, nl, tenv))
        end
in 

fun tcc_env(x, ol, nl, tenv) =
  (let fun checkTCVAR tyc = case (tc_outX tyc) of
       TC_VAR(i,j) => (case tcLookup(i,tenv) 
			of (SOME ts, _) => if j >= length ts 
					   then (print "tcc_env TC_VAR ";
						 print (Int.toString j);
						 print " ts length ";
						 print (Int.toString (length ts));
						 raise Fail "Bad TC_ENV TC_VAR")
					   else ()
			 | _ => ())
     | TC_ENV(tc, _, _, _)  => (print "TC_ENV("; checkTCVAR(tc); print ")\n")
     | _ => () (* print ("tcc_env OTHER " ^ tci_print tci ^"\n") *) 
   in checkTCVAR(x); 
    let val tvs = tc_vs x
   in case tvs
       of NONE => tcc_env_int(x, ol, nl, tenv)
        | SOME [] => x
        | SOME nvs => if withEff(nvs, ol, nl, tenv) 
                      then tcc_env_int(x, ol, nl, tenv)
                      else x 
    end
   end)

fun ltc_env(x, ol, nl, tenv) = 
  let val tvs = lt_vs x
   in case tvs
       of NONE => ltc_env_int(x, ol, nl, tenv)
        | SOME [] => x
        | SOME nvs => if withEff (nvs, ol, nl, tenv) 
                      then ltc_env_int(x, ol, nl, tenv)
                      else x 
  end

end (* local -- utility functions for lt_env and tc_env *)


(***************************************************************************
 *            UTILITY FUNCTIONS ON REASONING ABOUT REDUCTIONS              *
 ***************************************************************************)

(** a list of constructor functions *)
val tcc_var = tc_injX o TC_VAR
val tcc_fn = tc_injX o TC_FN
val tcc_app = fn (fntyc, argtycs) =>
		 (* Check that parameter arity matches number of arguments
		    supplied because type application must be saturated *) 
		 let fun checkParamArity (tc,tcs) = 
			 let 
			     fun getArity(tycEnv) =
				 (case (tc_outX tycEnv)
				   of TC_PRIM(ptyc) => PT.pt_arity ptyc
				    | TC_FN(params, _) => length params
				    | (TC_APP(tc, _)) => 
				      (case (tc_outX tc)
					of (TC_FN(_, tc')) => getArity tc'
					 | _ => 0)
				    | (TC_FIX((numFamily,tc,freetycs),index)) => 
				      (case (tc_outX tc) of
					   (TC_FN (_,tc')) => (* generator function *)
					   (case (tc_outX tc') of
						(TC_SEQ tycs) => getArity (List.nth (tycs, index))
					      | TC_FN (params, _) => length params
					      | _ => raise Fail "Malformed generator range")
					 | _ => raise Fail "FIX without generator!" )
				    | _ => (with_pp (fn s =>
                                              (PU.pps s "getArity?:";
                                               PP.newline s;
                                               ppTyc (!dp) s tc;
                                               PP.newline s));
                                            0))  (* giving up! *)
			     val arity = getArity tc
			 in
			     if arity = (length tcs) then ()
			     else with_pp(fn s =>
                                    (PU.pps s "TC_APP arity mismatch"; PP.newline s;
                                     PU.pps s "arity: "; PU.ppi s arity; PP.newline s;
                                     PU.pps s "no. arguments: "; PU.ppi s (length tcs);
                                     PP.newline s;
                                     PU.pps s "operator:"; PP.newline s;
                                     ppTyc (!dp) s tc; PP.newline s) )
			 end
		 in
		     (checkParamArity(fntyc, argtycs); 
		      (tc_injX o TC_APP) (fntyc, argtycs))
		 end
val tcc_seq = tc_injX o TC_SEQ
val tcc_proj = tc_injX o TC_PROJ
val tcc_fix = tc_injX o TC_FIX
val tcc_abs = tc_injX o TC_ABS
val tcc_tup  = tc_injX o TC_TUPLE
val tcc_parw = tc_injX o TC_PARROW
val tcc_box = tc_injX o TC_BOX
val tcc_real = tc_injX (TC_PRIM PT.ptc_real)
val ltc_tyc = lt_injX o LT_TYC
val ltc_str = lt_injX o LT_STR
val ltc_fct = lt_injX o LT_FCT
val ltc_poly = lt_injX o LT_POLY
val tcc_sum = tc_injX o TC_SUM
val tcc_token = tc_injX o TC_TOKEN

(* The following functions decide on how to flatten the arguments 
 * and results of an arbitrary FLINT function. The current threshold
 * is maintained by the "flatten_limit" parameter. This parameter
 * is designed as architecture independent, however, some implicit
 * constraints are:
 *     (1) flatten_limit <= numgpregs - numcalleesaves - 3
 *     (2) flatten_limit <= numfpregs - 2
 * Right now (2) is in general not true for x86; we inserted a 
 * special hack at cpstrans phase to deal with this case. In the
 * long term, if the spilling phase in the backend can offer more
 * supports on large-number of arguments, then we can make this
 * flattening more aggressive. (ZHONG)
 *) 
val flatten_limit = 9  

(* tcUnbound2 -- raised when second index of a deBruijn index pair is
 * out of bounds *)
exception tcUnbound2

fun isKnown tc = 
  (case tc_outX(tc_whnm tc)
    of (TC_PRIM _ | TC_ARROW _ | TC_BOX _ | TC_ABS _ | TC_PARROW _) => true 
     | (TC_CONT _ | TC_FIX _ | TC_SUM _ | TC_TUPLE _) => true
     | TC_APP(tc, _) => isKnown tc
     | TC_PROJ(tc, _) => isKnown tc
     | TC_TOKEN(k, x) => token_isKnown(k, x)
     | _ => false)

and tc_autoflat tc = 
  let val ntc = tc_whnm tc 
   in (case tc_outX ntc
        of TC_TUPLE (_, [_]) => (* singleton record is not flattened to ensure
                              isomorphism btw plambdatype and flinttype *)
             (true, [ntc], false)
         | TC_TUPLE (_, []) =>  (* unit is not flattened to avoid coercions *)
             (true, [ntc], false)
         | TC_TUPLE (_, ts) => 
             if length ts <= flatten_limit then (true, ts, true)
             else (true, [ntc], false)  (* ZHONG added the magic number 10 *)
         | _ => if isKnown ntc then (true, [ntc], false)
                else (false, [ntc], false))
  end

and tc_autotuple [x] = x 
  | tc_autotuple xs = 
       if length xs <= flatten_limit then tcc_tup (RF_TMP, xs)
       else bug "fatal error with tc_autotuple"

and tcs_autoflat (flag, ts) = 
  if flag then (flag, ts) 
  else (case ts 
         of [tc] => (let val ntc = tc_whnm tc
                         val (nraw, ntcs, _) = tc_autoflat ntc
                      in (nraw, ntcs)
                     end)
          | _ => bug "unexpected cooked multiples in tcs_autoflat")

and lt_autoflat lt = 
  (case lt_outX(lt_whnm lt)
    of LT_TYC tc => 
         let val (raw, ts, flag) = tc_autoflat tc
          in (raw, map ltc_tyc ts, flag)
         end
     | _ => (true, [lt], false))

(** a special version of tcc_arw that does automatic flattening *)
and tcc_arw (x as (FF_FIXED, _, _)) = tc_injX (TC_ARROW x)
  | tcc_arw (x as (FF_VAR (true, true), _, _)) = tc_injX (TC_ARROW x)
  | tcc_arw (b as (FF_VAR (b1, b2)), ts1, ts2) =
      let val (nb1, nts1) = tcs_autoflat (b1, ts1)
          val (nb2, nts2) = tcs_autoflat (b2, ts2)
       in tc_injX (TC_ARROW(FF_VAR(nb1, nb2),  nts1, nts2))
      end

(** utility function to read the top-level of a tyc *)
and tc_lzrd(t: tyc) = 
  let fun g x = 
            (case tc_outX x
              of TC_IND (tc, _) => g tc
               | TC_ENV (tc, i, j, te) => 
                   let val ntc = g(h(tc, i, j, te))
                    in tyc_upd(x, ntc); ntc
                   end
               | _ => x)

      and h (x, 0, 0, _) = g x
        | h (x, ol, nl, tenv) = 
            let fun prop z = tcc_env(z, ol, nl, tenv) 
		             handle Fail _ =>
                               (with_pp(fn s =>
                                 (PU.pps s "tc_lzrd.prop:"; PP.newline s;
                                  ppTyc (!dp) s z; PP.newline s));
                                raise Fail ("tc_lzrd prop"))
             in (case tc_outX x
                  of TC_VAR (i,j) => 
                       if (i <= ol) then  (* i is bound in tenv *)
                         (case tcLookup(i, tenv) 
                           of (NONE, n) => tcc_var(nl - n, j) (* rule r5 *)
                            | (SOME ts, n) =>  
                                 let val y = List.nth(ts, j) 
                                             handle Subscript => 
                    (with_pp(fn s =>
                       let val {break,newline,openHVBox,openHOVBox,closeBox,
                                pps, ppi} = PU.en_pp s
                       in openHVBox 0;
                          pps "***Debugging***"; newline();
                          pps "tc_lzrd arg: "; PPLty.ppTyc (!dp) s t; newline();
		          pps "i = "; ppi i; pps ", j = "; ppi j; newline();
                          pps "length(ts) = : "; ppi (length ts); newline();
                          pps "ts elements: "; break{nsp=2,offset=2};
                          openHOVBox 2;
                          ppList s {sep=",",pp=ppTyc (!dp)} ts;
                          closeBox ();
                          closeBox ();
                          newline(); PP.flushStream s
			end);
			raise tcUnbound2)
                                 in h(y, 0, nl - n, initTycEnv)  (* rule r6 *)
                                 end)
                       else tcc_var(i-ol+nl, j) (* rule r4 *)
                   | TC_NVAR _ => x
                   | TC_PRIM _ => x    (* rule r7 *)
                   | TC_FN (ks, tc) => 
                      let val tenv' = tcInsert(tenv, (NONE, nl))
                       in tcc_fn(ks, 
				 tcc_env(tc, ol+1, nl+1, tenv') 
				 handle Fail _ => raise Fail "tc_lzrd TC_FN") (* rule r10 *)
                      end
                   | TC_APP (tc, tcs) => tcc_app(prop tc, map prop tcs) (* rule r9 *)
                   | TC_SEQ tcs => tcc_seq (map prop tcs)
                   | TC_PROJ (tc, i) => tcc_proj(prop tc, i)
                   | TC_SUM tcs => tcc_sum (map prop tcs)
                   | TC_FIX ((n,tc,ts), i) => 
                        tcc_fix((n, prop tc, map prop ts), i)
                   | TC_ABS tc => tcc_abs (prop tc)
                   | TC_BOX tc => tcc_box (prop tc)
                   | TC_TUPLE (rk, tcs) => tcc_tup (rk, map prop tcs)
                   | TC_ARROW (r, ts1, ts2) => 
                       tcc_arw (r, map prop ts1, map prop ts2)  (* rule r8 *)
                   | TC_PARROW (t1, t2) => tcc_parw (prop t1, prop t2)
                   | TC_TOKEN (k, t) => tcc_token(k, prop t)
                   | TC_CONT _ => bug "unexpected TC_CONT in tc_lzrd"
                   | TC_IND (tc, _) => h(tc, ol, nl, tenv)
                   | TC_ENV(tc, ol', nl', tenv') => 
                       if ol = 0 then h(tc, ol', nl+nl', tenv')  (* rule r11 *)
                       else h(g x, ol, nl, tenv))
            end (* function h *)
   in if tcp_norm(t) then t else g t
  end (* function tc_lzrd *)

(** utility function to read the top-level of an lty *)
and lt_lzrd t = 
  let fun g x = 
           (case lt_outX x
             of LT_IND (lt, _) => g lt
              | LT_ENV(lt, i, j, te) => 
                  let val nlt = g(h(lt, i, j, te))
                   in lty_upd(x, nlt); nlt
                  end
              | _ => x)

      and h (x, 0, 0, _) = g x
        | h (x, ol, nl, tenv) = 
            let fun prop z = ltc_env(z, ol, nl, tenv)
             in (case lt_outX x
                  of LT_TYC tc => ltc_tyc (tcc_env(tc, ol, nl, tenv))
                   | LT_STR ts => ltc_str (map prop ts)
                   | LT_FCT (ts1, ts2) => ltc_fct(map prop ts1, map prop ts2)
                   | LT_POLY (ks, ts) => 
                       let val tenv' = tcInsert(tenv, (NONE, nl))
                        in ltc_poly(ks, 
                             map (fn t => ltc_env(t, ol+1, nl+1, tenv')) ts)
                       end
                   | LT_CONT _ => bug "unexpected LT_CONT in lt_lzrd"
                   | LT_IND (t, _) => h(t, ol, nl, tenv)
                   | LT_ENV (lt, ol', nl', tenv') => 
                       if ol = 0 then h(lt, ol', nl+nl', tenv')
                       else h(g x, ol, nl, tenv))
            end (* function h *)
   in if ltp_norm(t) then t else g t
  end (* function lt_lzrd *)

(** taking out the TC_IND indirection *)
and stripInd t = (case tc_outX t of TC_IND (x,_) => stripInd x | _ => t)

(*
and printParamArgs (tc,tcs) = 
    let fun getArity(tycEnv) =
	    (case (tc_outX tycEnv) of
		 TC_PRIM(ptyc) => PT.pt_arity ptyc
	       | TC_FN(params, _) => length params
	       | (TC_APP(tc, _)) => 
		 (case (tc_outX tc)
		   of (TC_FN(_, tc')) => getArity tc'
		    | _ => 0)
	       | (TC_FIX((numFamily,tc,freetycs),index)) => 
		 (case (tc_outX tc) of
		      (TC_FN (_,tc')) => (* generator function *)
		      (case (tc_outX tc') of
			   (TC_SEQ tycs) => getArity (List.nth (tycs, index))
			 | TC_FN (params, _) => length params
			 | _ => raise Fail "Malformed generator range")
		    | _ => raise Fail "FIX without generator!" )
	       | _ => (with_pp (fn s => (PP.openHOVBox s (PP.Rel 2);
                                         PU.pps s "getArity on:";
                                         ppTyc (!dp) s tc; PP.newline s;
                                         PP.closeBox s));
                       0))
	val arity = getArity tc
    in
	if arity = (length tcs) then ()
	else with_pp(fn s =>
                        (PU.pps s "(TC_APP oper:"; PP.break s {nsp=1,offset=2};
                         ppTyc (!dp) s tc; PP.newline s;
                         PU.pps s "arity:"; PP.break s {nsp=1,offset=0};
		         PU.ppi s arity; PP.newline s;
		         PU.pps s "no. arguments:"; PP.break s {nsp=1,offset=0};
		         PU.ppi s (length tcs); PP.newline s))
    end
 *)
(** normalizing an arbitrary tyc into a simple weak-head-normal-form *)
and tc_whnm t = if tcp_norm(t) then t else 
  let (* val _ = print ">>tc_whnm not norm\n" *) 
      val nt = tc_lzrd t
   in case (tc_outX nt)
       of TC_APP(tc, tcs) =>
	    ((* print "\ntc_whnm: TC_APP\n"; *)
            (let val tc' = tc_whnm tc handle Fail _ => raise Fail "TC_APP in tc_whnm 1"
              in case (tc_outX tc')
                  of TC_FN(ks, b) =>  
                       let fun base () = 
                             (b, 1, 0, tcInsert(initTycEnv,(SOME tcs, 0)))
                           val sp = 
                             (case tc_outX b
                               of TC_ENV(b', ol', nl', te') => 
                                    (case tcSplit te'
                                      of SOME((NONE, n), te) =>
                                           if (n = nl'-1) andalso (ol' > 0)
                                           then (b', ol', n, 
                                                 tcInsert(te, (SOME tcs, n)))
                                           else base()
                                       | _ => base())
                                | _ => base()) 
                           val res = tc_whnm(tcc_env sp) 
			             handle Fail _ => raise Fail "TC_APP in tc_whnm 2" 
                        in tyc_upd(nt, res); res
                       end
                   | ((TC_SEQ _) | (TC_TUPLE _) | (TC_ARROW _) | (TC_IND _)) =>
                       bug "unexpected tycs in tc_whnm-TC_APP"
                   | _ => let val xx = tcc_app(tc', tcs) 
                           in stripInd xx
                          end
             end))
        | TC_PROJ(tc, i) =>
	   ((* print "\ntc_whnm: TC_PROJ\n"; *) 
	   (let val tc' = tc_whnm tc
              in (case (tc_outX tc')
                   of (TC_SEQ tcs) => 
                        let val res = List.nth(tcs, i)
                                      handle _ => bug "TC_SEQ in tc_whnm"
                            val nres = tc_whnm res
                         in tyc_upd(nt, nres); nres
                        end
                    | ((TC_PRIM _) | (TC_NVAR _) | (TC_FIX _) | (TC_FN _) |
                       (TC_SUM _) | (TC_ARROW _) | (TC_ABS _) | (TC_BOX _) | 
                       (TC_IND _) | (TC_TUPLE _)) =>
                         bug "unexpected tycs in tc_whnm-TC_PROJ"
                    | _ => let val xx = tcc_proj(tc', i)
                            in stripInd xx
                           end)
             end))
        | TC_TOKEN(k, tc)  =>
	    ((* print "\ntc_whnm: TC_TOKEN\n"; *)
	    (let val tc' = tc_whnm tc
              in if token_whnm k tc' 
                 then let val xx = tcc_token(k, tc') in stripInd xx end
                 else let val res = token_reduce(k, tc')
                          val nres = tc_whnm res
                       in tyc_upd(nt, nres); nres
                      end
             end))
        | TC_IND (tc, _) => ((*print "\ntc_whnm: TC_IND\n"; *) tc_whnm tc)
        | TC_ENV _ => bug "unexpected TC_ENV in tc_whnm"
        | _ => ((* print "\ntc_whnm: OTHER\n"; *) nt)
  end (* function tc_whnm *)

(** normalizing an arbitrary lty into the simple weak-head-normal-form *)
and lt_whnm t = if ltp_norm(t) then t else 
  let val nt = lt_lzrd t
   in case (lt_outX nt)
       of LT_TYC tc => ltc_tyc(tc_whnm tc)
        | _ => nt
  end (* function lt_whnm *)

(** normalizing an arbitrary tyc into the standard normal form *)
fun tc_norm t = if (tcp_norm t) then t else
  let val nt = tc_whnm t
   in if (tcp_norm nt) then nt
      else
        (let val res = 
              (case (tc_outX nt)
                of TC_FN (ks, tc) => tcc_fn(ks, tc_norm tc)
                 | TC_APP (tc, tcs) => tcc_app(tc_norm tc, map tc_norm tcs)
                 | TC_SEQ tcs => tcc_seq(map tc_norm tcs)
                 | TC_PROJ (tc, i) => tcc_proj(tc_norm tc, i)
                 | TC_SUM tcs => tcc_sum (map tc_norm tcs)
                 | TC_FIX ((n,tc,ts), i) => 
                     tcc_fix((n, tc_norm tc, map tc_norm ts), i)
                 | TC_ABS tc => tcc_abs(tc_norm tc)
                 | TC_BOX tc => tcc_box(tc_norm tc)
                 | TC_TUPLE (rk, tcs) => tcc_tup(rk, map tc_norm tcs)
                 | TC_ARROW (r, ts1, ts2) => 
                     tcc_arw(r, map tc_norm ts1, map tc_norm ts2)
                 | TC_PARROW (t1, t2) => tcc_parw(tc_norm t1, tc_norm t2)
                 | TC_TOKEN (k, t) => tcc_token(k, tc_norm t)
                 | TC_IND (tc, _) => tc_norm tc
                 | TC_ENV _ => bug "unexpected tycs in tc_norm"
                 | _ => nt)
          in tyc_upd(nt, res); res
         end)
  end (* function tc_norm *)

(** normalizing an arbitrary lty into the standard normal form *)
fun lt_norm t = if (ltp_norm t) then t else 
  let val nt = lt_lzrd t
   in if (ltp_norm nt) then nt
      else 
        (let val res = 
              (case lt_outX nt
                of LT_TYC tc => ltc_tyc(tc_norm tc)
                 | LT_STR ts => ltc_str(map lt_norm ts)
                 | LT_FCT (ts1, ts2) => 
                     ltc_fct(map lt_norm ts1, map lt_norm ts2)
                 | LT_POLY (ks, ts) => ltc_poly (ks, map lt_norm ts)
                 | LT_IND (lt, _) => lt_norm lt
                 | _ => bug "unexpected ltys in lt_norm")
          in lty_upd(nt, res); res
         end)
  end (* function lt_norm *)

(***************************************************************************
 *         REGISTER A NEW TOKEN TYC --- TC_WRAP                            *
 ***************************************************************************)

(** we add a new constructor named TC_RBOX through the token facility *)
local val name = "TC_WRAP"
      val abbrev = "WR"
      val is_known = fn _ => true      (* why is this ? *)
      fun tcc_tok k t = tcc_token(k, t)

      fun unknown tc = 
        (case tc_outX tc
          of (TC_VAR _ | TC_NVAR _) => true
           | (TC_APP(tc, _)) => unknown tc
           | (TC_PROJ(tc, _)) => unknown tc
           | _ => false)         

      fun flex_tuple ts = 
        let fun hhh(x::r, ukn, wfree) = 
                 let fun iswp tc =
                       (case tc_outX tc
                         of TC_TOKEN(k', t) => (* WARNING: need check k' *)
                              (case tc_outX t
                                of TC_PRIM pt => false
                                 | _ => true)
                          | _ => true)
                  in hhh(r, (unknown x) orelse ukn, (iswp x) andalso wfree)
                 end
              | hhh([], ukn, wfree) = ukn andalso wfree
         in hhh(ts, false, true)
        end

      fun is_whnm tc = 
        (case tc_outX tc
          of (TC_ARROW(FF_FIXED, [t], _)) => (unknown t) 
           | (TC_TUPLE(rf, ts)) => flex_tuple ts
           | (TC_PRIM pt) => PT.unboxed pt
           | _ => false)

      (* invariants: tc itself is in whnm but is_whnm tc = false *)
      fun reduce_one (k, tc) =  
        (case tc_outX tc
          of TC_TUPLE (rk, ts) => 
               let fun hhh (x::r, nts, ukn) = 
                         let val nx = tc_whnm x
                             val b1 = unknown nx
                             val nnx = 
                               (case tc_outX nx
                                 of TC_TOKEN(k', t) =>
                                      if token_eq(k, k') then
                                        (case tc_outX t 
                                          of TC_PRIM _ => t
                                           | _ => nx)
                                      else nx
                                  | _ => nx)
                          in hhh(r, nnx::nts, b1 orelse ukn)
                         end
                     | hhh ([], nts, ukn) = 
                         let val nt = tcc_tup(rk, rev nts)
                          in if ukn then tcc_token(k, nt) else nt
                         end
                in hhh(ts, [], false)
               end
           | TC_ARROW (FF_FIXED, [_,_], [_]) => tc
           | TC_ARROW (FF_FIXED, [t1], ts2 as [_]) => 
               let val nt1 = tc_whnm t1
                   fun ggg z = 
                     let val nz = tc_whnm z
                      in (case tc_outX nz
                           of TC_PRIM pt => 
                                if PT.unboxed pt then tcc_token(k, nz)
                                else nz
                            | _ => nz)
                     end
                   val (wp, nts1) =
                     (case tc_outX nt1
                       of TC_TUPLE(_, [x,y]) => (false, [ggg x, ggg y])
                        | TC_TOKEN(k', x) => 
                            if token_eq(k, k') then
                              (case (tc_outX x)
                                of TC_TUPLE(_, [y, z]) => 
                                    (false, [ggg y, ggg z])
                                 | _ => (false, [nt1]))
                            else (false, [nt1])
                        | _ => (unknown nt1, [nt1]))
                   val nt = tcc_arw(FF_FIXED, nts1, ts2)
                in if wp then tcc_token(k, nt) else nt
               end
           | TC_ARROW (FF_FIXED, _, _) => 
               bug "unexpected reduce_one on ill-formed FF_FIX arrow types"
           | TC_ARROW (FF_VAR(b1,b2), ts1, ts2) => 
               bug "calling reduce_one on FF_VAR arrow types"
           | TC_PRIM pt => 
               if PT.unboxed pt then 
                 bug "calling reduce_one on an already-reduced whnm"
               else tc
           | TC_TOKEN(k', t) =>
               if token_eq(k, k') then tc
               else bug "unexpected token in reduce_one"
           | (TC_BOX _ | TC_ABS _ | TC_PARROW _) => 
               bug "unexpected tc_box/abs/parrow in reduce_one"
           | TC_ENV _ => bug "unexpected TC_ENV in reduce_one"
           | TC_IND _ => bug "unexpected TC_IND in reduce_one"
           | _ => tc)

in

val wrap_token = 
  register_token {name=name, abbrev=abbrev, reduce_one=reduce_one,
                  is_whnm=is_whnm, is_known=is_known}

end (* end of creating the box token for "tcc_rbox" *)

(** testing if a tyc is a unknown constructor *)
fun tc_unknown tc = not (isKnown tc)

(***************************************************************************
 *         REBINDING THE INJECTION AND PROJECTION FUNCTIONS                *
 ***************************************************************************)
(** converting from the standard reps to the hash-consing reps *)
val tk_inj = tk_injX
val tc_inj = tc_injX
val lt_inj = lt_injX

(** converting from the hash-consing reps to the standard reps *)
val tk_out = tk_outX 
val tc_out = tc_outX o tc_whnm
val lt_out = lt_outX o lt_whnm

(***************************************************************************
 *         UTILITY FUNCTIONS ON TESTING EQUIVALENCE                        *
 ***************************************************************************)

(** testing the equality of values of tkind, tyc, lty *)
fun eqlist p (x::xs, y::ys) = (p(x,y)) andalso (eqlist p (xs, ys))
  | eqlist p ([], []) = true
  | eqlist _ _ = false

(** testing the equivalence for arbitrary tkinds, tycs and ltys *)
val tk_eqv = tk_eq       (* all tkinds are normalized *)

local (* tyc equivalence utilities *)
(* The efficiency of checking FIX equivalence could probably be
 * improved somewhat, but it doesn't seem so bad for my purposes right
 * now.  Anyway, somebody might eventually want to do some profiling
 * and improve this.  --league, 24 March 1998
 *)
    
(* Profiling code, temporary?? *)
structure Click =
struct
    local
        val s_unroll = Stats.makeStat "FIX unrolls"
    in
        fun unroll() = Stats.addStat s_unroll 1
    end
end (* Click *)

(** unrolling a fix, tyc -> tyc *)
fun tc_unroll_fix tyc =
    case tc_outX tyc of
        (TC_FIX((n,tc,ts),i)) => let
            fun genfix i = tcc_fix ((n,tc,ts),i)
            val fixes = List.tabulate(n, genfix)
            val mu = tc
            val mu = if null ts then mu
                     else tcc_app (mu,ts)
            val mu = tcc_app (mu, fixes)
            val mu = if n=1 then mu
                     else tcc_proj (mu, i)
        in
            Click.unroll();
            mu
        end
      | _ => bug "unexpected non-FIX in tc_unroll_fix"

(* In order to check equality of two FIXes, we need to be able to
 * unroll them once, and check equality on the unrolled version, with
 * an inductive assumption that they ARE equal.  The following code
 * supports making and checking these inductive assumptions.
 * Furthermore, we need to avoid unrolling any FIX more than once.
 *)
structure TcDict = RedBlackMapFn
                       (struct
                           type ord_key = tyc
                           val compare = tc_cmp
                       end)
(* for each tyc in this dictionary, we store a dictionary containing
 * tycs that are assumed equivalent to it.
 *)
type eqclass = unit TcDict.map
type hyp = eqclass TcDict.map

(* the null hypothesis, no assumptions about equality *)
val empty_eqclass : eqclass = TcDict.empty
val null_hyp : hyp = TcDict.empty

(* add assumption t1=t2 to current hypothesis.  returns composite
 * hypothesis.
 *)
fun assume_eq' (hyp, t1, t1eqOpt, t2) = let
    val t1eq  = case t1eqOpt of SOME e => e | NONE => empty_eqclass
    val t1eq' = TcDict.insert (t1eq, t2, ())
    val hyp'  = TcDict.insert (hyp, t1, t1eq')
in
    hyp'
end

fun assume_eq (hyp, t1, t1eqOpt, t2, t2eqOpt) =
    assume_eq' (assume_eq' (hyp, t1, t1eqOpt, t2),
                t2, t2eqOpt, t1)

(* check whether t1=t2 according to the hypothesis *)
val eq_by_hyp : eqclass option * tyc -> bool
    = fn (NONE, t2) => false
       | (SOME eqclass, t2) =>
         isSome (TcDict.find (eqclass, t2))
    
(* have we made any assumptions about `t' already? *)
val visited : eqclass option -> bool 
  = isSome

(* testing if two recursive datatypes are equivalent *)
fun eq_fix (eqop1, hyp) (t1, t2) = 
  (case (tc_outX t1, tc_outX t2) 
    of (TC_FIX((n1,tc1,ts1),i1), TC_FIX((n2,tc2,ts2),i2)) => 
        if not (!Control.FLINT.checkDatatypes) then true 
        else let 
            val t1eqOpt = TcDict.find (hyp, t1)
        in
            (* first check the induction hypothesis.  we only ever
             * make hypotheses about FIX nodes, so this test is okay
             * here.  if assume_eq appears in other cases, this 
             * test should be lifted outside the switch.
             *)
            if eq_by_hyp (t1eqOpt, t2) then true
            (* next try structural eq on the components.  i'm not sure why
             * this part is necessary, but it does seem to be... --league,
             * 23 March 1998
             *)
            else
                (n1 = n2 andalso i1 = i2 andalso
                 eqop1 hyp (tc1, tc2) andalso 
                 eqlist (eqop1 hyp) (ts1, ts2)) orelse
                (* not equal by inspection; we have to unroll it.
                 * we prevent unrolling the same FIX twice by asking
                 * the `visited' function.
                 *)
                if visited t1eqOpt then false 
                else let
                    val t2eqOpt = TcDict.find (hyp, t2)
                in
                    if visited t2eqOpt then false 
                    else eqop1 (assume_eq (hyp, t1, t1eqOpt,
                                           t2, t2eqOpt))
                               (tc_unroll_fix t1, tc_unroll_fix t2)
                end
        end
     | _ => bug "unexpected types in eq_fix")


(* tc_eqv_generator, invariant: t1 and t2 are in the wh-normal form 
 *     eqop1 is the default equality to be used for tycs
 *     eqop2 is used for body of FN, arguments in APP,
 *     eqop3 is used for ABS and BOX.
 *     eqop4 is used for arrow arguments and results
 * Each of these first takes the set of hypotheses.
 *)
fun tc_eqv_gen (eqop1, eqop2, hyp) (t1, t2) = 
    case (tc_outX t1, tc_outX t2) of
        (TC_FIX _, TC_FIX _) => eqop2 (eqop1, hyp) (t1, t2)
      | (TC_FN(ks1, b1), TC_FN(ks2, b2)) =>
        eqlist tk_eqv (ks1, ks2) andalso eqop1 hyp (b1, b2)
      | (TC_APP(a1, b1), TC_APP(a2, b2)) =>
        eqop1 hyp (a1, a2) andalso eqlist (eqop1 hyp) (b1, b2)
      | (TC_SEQ ts1, TC_SEQ ts2) =>
        eqlist (eqop1 hyp) (ts1, ts2)
      | (TC_SUM ts1, TC_SUM ts2) =>
        eqlist (eqop1 hyp) (ts1, ts2)
      | (TC_TUPLE (_, ts1), TC_TUPLE (_, ts2)) =>
        eqlist (eqop1 hyp) (ts1, ts2)
      | (TC_ABS a, TC_ABS b) =>
        eqop1 hyp (a, b)
      | (TC_BOX a, TC_BOX b) =>
        eqop1 hyp (a, b)
      | (TC_TOKEN(k1,t1), TC_TOKEN(k2,t2)) => 
        token_eq(k1,k2) andalso eqop1 hyp (t1,t2)
      | (TC_PROJ(a1, i1), TC_PROJ(a2, i2)) =>
        i1 = i2 andalso eqop1 hyp (a1, a2)
      | (TC_ARROW(r1, a1, b1), TC_ARROW(r2, a2, b2)) => 
        r1 = r2 andalso eqlist (eqop1 hyp) (a1, a2) 
                andalso eqlist (eqop1 hyp) (b1, b2)
      | (TC_PARROW(a1, b1), TC_PARROW(a2, b2)) => 
        eqop1 hyp (a1, a2) andalso eqop1 hyp (b1, b2)
      | (TC_CONT ts1, TC_CONT ts2) =>
        eqlist (eqop1 hyp) (ts1, ts2)
      | _ => false

(** general equality for tycs *)
fun tc_eqv' hyp (x, y) =
    let val t1 = tc_whnm x
        val t2 = tc_whnm y
    in
        if tcp_norm t1 andalso tcp_norm t2 then tc_eq (t1, t2)
        else tc_eqv_gen (tc_eqv', fn _ => tc_eq, hyp) (t1, t2)
    end (* tc_eqv' *)

(* slightly relaxed constraints (???) *)
fun tc_eqv_x' hyp (x, y) =
    let val t1 = tc_whnm x
        val t2 = tc_whnm y
     in (if (tcp_norm t1) andalso (tcp_norm t2) then tc_eq(t1, t2)
         else false) orelse
         (tc_eqv_gen (tc_eqv_x', eq_fix, hyp) (t1, t2))
    end (* function tc_eqv_x' *)

in (* tyc equivalence utilities *)

val tc_eqv = tc_eqv' null_hyp
val tc_eqv_x = tc_eqv_x' null_hyp

end (* tyc equivalence utilities *)


(** lt_eqv_generator, invariant: x and y are in the wh-normal form *)
fun lt_eqv_gen (eqop1, eqop2) (x : lty, y) = 
  let (* seq should be called if t1 and t2 are weak-head normal form *)
      fun seq (t1, t2) = 
        (case (lt_outX t1, lt_outX t2)
          of (LT_POLY(ks1, b1), LT_POLY(ks2, b2)) =>
               (eqlist tk_eqv (ks1, ks2)) andalso (eqlist eqop1 (b1, b2))
           | (LT_FCT(as1, bs1), LT_FCT(as2, bs2)) => 
               (eqlist eqop1 (as1, as2)) andalso (eqlist eqop1 (bs1, bs2))
           | (LT_TYC a, LT_TYC b) => eqop2(a, b)
           | (LT_STR s1, LT_STR s2) => eqlist eqop1 (s1, s2)
           | (LT_CONT s1, LT_CONT s2) => eqlist eqop1 (s1, s2)
           | _ => false)
   in seq(x, y)
  end (* function lt_eqv_gen *)

fun lt_eqv(x : lty, y) = 
  let val seq = lt_eqv_gen (lt_eqv, tc_eqv) 
   in if ((ltp_norm x) andalso (ltp_norm y)) then lt_eq(x,y)
      else (let val t1 = lt_whnm x
                val t2 = lt_whnm y
             in if (ltp_norm t1) andalso (ltp_norm t2) then lt_eq(x, y)
                else seq(t1, t2)
            end)
  end (* function lt_eqv *)

fun lt_eqv_x(x : lty, y) = 
  let val seq = lt_eqv_gen (lt_eqv_x, tc_eqv_x) 
   in if ((ltp_norm x) andalso (ltp_norm y)) then 
           (lt_eq(x, y)) orelse (seq(x, y))
      else (let val t1 = lt_whnm x
                val t2 = lt_whnm y
             in if (ltp_norm t1) andalso (ltp_norm t2) then 
                  (lt_eq(t1, t2)) orelse (seq(t1, t2))
                else seq(t1, t2)
            end)
  end (* function lt_eqv *)

(** testing equivalence of fflags and rflags *)
fun ff_eqv (FF_VAR (b1, b2), FF_VAR (b1', b2')) = b1 = b1' andalso b2 = b2'
  | ff_eqv (FF_FIXED, FF_FIXED) = true
  | ff_eqv ((FF_FIXED, FF_VAR _) | (FF_VAR _, FF_FIXED)) = false

fun rf_eqv (RF_TMP, RF_TMP) = true

(***************************************************************************
 *  UTILITY FUNCTIONS ON FINDING OUT THE DEPTH OF THE FREE TYC VARIABLES   *
 ***************************************************************************)
(** finding out the innermost binding depth for a tyc's free variables *)
fun tc_depth (x, d) =
      (* unfortunately we have to reduce everything to normal form
       * before we can talk about its list of free type variables. *)
    (case tc_vs (tc_norm x)
       of NONE => bug "unexpected case in tc_depth"
        | SOME [] => DI.top
        | SOME (a::_) => d + 1 - (#1(tvDecode a)))

fun tcs_depth ([], d) = DI.top
  | tcs_depth (x::r, d) = Int.max(tc_depth(x, d), tcs_depth(r, d))

(* these return the list of free NAMED tyvars, after nomalization *)
fun tc_nvars (tyc:tyc) =
    Lty.tc_nvars(tc_norm tyc)

fun lt_nvars (lty:lty) =
    Lty.lt_nvars(lt_norm lty)

end (* abstraction LtyKernel *)
