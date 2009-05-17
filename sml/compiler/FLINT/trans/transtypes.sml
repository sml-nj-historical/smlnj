(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* transtypes.sml *)

signature TRANSTYPES = 
sig

  type flexmap = TycPath.tycpath FlexTycMap.map 
  datatype primary = FormalTyc of Types.tycon
		   | FormalFct of Stamps.stamp * Modules.fctSig

  val genTT  : unit
	       -> {tpsKnd : primary -> PLambdaType.tkind,
                   tpsTyc : flexmap -> DebIndex.depth -> TycPath.tycpath 
                            -> PLambdaType.tyc,
                   toTyc  : flexmap -> 
			    DebIndex.depth -> Types.ty -> PLambdaType.tyc,
                   toLty  : flexmap -> DebIndex.depth -> Types.ty 
			    -> PLambdaType.lty,
                   strLty : flexmap * Modules.Structure 
			    * DebIndex.depth 
                            * ElabUtil.compInfo -> PLambdaType.lty,
                   fctLty : flexmap * Modules.Functor 
			    * DebIndex.depth 
                            * ElabUtil.compInfo -> PLambdaType.lty}
end (* signature TRANSTYPES *)

structure TransTypes : TRANSTYPES = 
struct
local structure BT = BasicTypes
      structure DA = Access   
      structure DI = DebIndex
      structure EE = EntityEnv
      structure EM = ErrorMsg
      structure EPC = EntPathContext
      structure EV = EvalEntity
      structure INS = Instantiate
      structure IP = InvPath
      structure LT = PLambdaType
      structure PT = PrimTyc
      structure MU = ModuleUtil
      structure SE = StaticEnv
      structure TU = TypesUtil
      structure PP = PrettyPrintNew
      structure TP = TycPath
      structure FTM = FlexTycMap

      open Types Modules ElabDebug
in

type flexmap = TycPath.tycpath FlexTycMap.map 

datatype primary = FormalTyc of Types.tycon
		 | FormalFct of Stamps.stamp * fctSig

fun bug msg = ErrorMsg.impossible ("TransTypes: " ^ msg)
val say = Control.Print.say 
val debugging = (* FLINT_Control.tmdebugging *) ref true
fun debugmsg (msg: string) =
  if !debugging then (say msg; say "\n") else ()
val debugPrint = (fn x => debugPrint debugging x)
val defaultError =
  EM.errorNoFile(EM.defaultConsumer(),ref false) SourceMap.nullRegion

val env = StaticEnv.empty

fun ppType x = 
 ((PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.string ppstrm "find: ";
                          PPType.resetPPType();
                          PPType.ppType env ppstrm x)))
  handle _ => say "fail to print anything")

fun ppTycon x = 
    ((PP.with_pp (EM.defaultConsumer())
        (fn ppstrm => (PP.string ppstrm "find: ";
                       PPType.resetPPType();
                       PPType.ppTycon env ppstrm x)))
    handle _ => say "fail to print anything")


fun ppLtyc ltyc = 
    PP.with_default_pp (fn ppstrm => PPLty.ppTyc 20 ppstrm ltyc)


(****************************************************************************
 *               TRANSLATING ML TYPES INTO FLINT TYPES                      *
 ****************************************************************************)
local val recTyContext = ref [~1]
in 
fun enterRecTy (a) = (recTyContext := (a::(!recTyContext)))
fun exitRecTy () = (recTyContext := tl (!recTyContext))
fun recTyc (i) = 
      let val x = hd(!recTyContext)
          val base = DI.innermost
       in if x = 0 then LT.tcc_var(base, i)
          else if x > 0 then LT.tcc_var(DI.di_inner base, i)
               else bug "unexpected RECtyc"
      end
fun freeTyc (i) = 
      let val x = hd(!recTyContext)
          val base = DI.di_inner (DI.innermost)
       in if x = 0 then LT.tcc_var(base, i)
          else if x > 0 then LT.tcc_var(DI.di_inner base, i)
               else bug "unexpected RECtyc"
      end
end (* end of recTyc and freeTyc hack *)

fun tpsKnd (FormalTyc(GENtyc{kind=FORMAL,arity,...})) =
    LT.tkc_int arity
  | tpsKnd (FormalFct(stamp, fsig as FSIG {kind,
				   paramsig,
				   paramvar,
				   paramsym,
				   bodysig})) =
    bug "modkind" 
  | tpsKnd _ = bug "unexpected primary in tpsKnd"

(* fun tpsKnd (TP.TP_VAR{kind,...}) = kind 
  | tpsKnd (TP.TP_FCT(argtps, bodytps)) = 
      LT.tkc_fun(map tpsKnd argtps, LT.tkc_seq (map tpsKnd bodytps))
  | tpsKnd (TP.TP_SEL(TP.TP_APP(TP.TP_VAR{kind,...}, paramtps), i)) =
      let val (_, result) = LT.tkd_fun kind
	  val seq = LT.tkd_seq result
	  val _ = debugmsg ("--tpsKnd fct result list length "^
			    Int.toString (length seq)^" selecting "^
			    Int.toString i) 
	  val knd = List.nth(seq, i) 
	      handle General.Subscript => 
		     bug ("Unexpected functor result length, selecting "
			  ^Int.toString i^" in length "
			  ^Int.toString(length seq)^" seq")
					    
      in knd
      end
  | tpsKnd _ = bug "unexpected tycpath parameters in tpsKnd" *)

fun genTT() = 
  let

(* fun tpsTyc (fm : flexmap) d tp = 
  let fun h (TP.TP_VAR {tdepth, num, ...}, cur) =
            let val finaldepth = DI.calc(cur, tdepth)
		val _ = debugmsg ("--tpsTyc: producing tcc_var "^
				DI.dp_print tdepth^" "
			   ^Int.toString num^" current depth "^DI.dp_print cur)
	    in
		if finaldepth < 0 then bug "Invalid depth calculation"
		else LT.tcc_var(finaldepth, num)
	    end
        | h (TP.TP_TYC tc, cur) = tycTyc(fm, tc, cur)
        | h (TP.TP_SEL (tp, i), cur) = LT.tcc_proj(h(tp, cur), i)
        | h (TP.TP_APP (tp, ps), cur) = 
              LT.tcc_app(h(tp, cur), map (fn x => h(x, cur)) ps)
        | h (TP.TP_FCT (ps, ts), cur) = 
             let val _ = debugmsg ">>tpsTyc[TP_FCT]"
		 val ks = map tpsKnd ps
                 val cur' = DI.next cur
                 val ts' = map (fn x => h(x, cur')) ts
		 val _ = debugmsg "<<tpsTyc[TP_FCT]"
             in LT.tcc_fn(ks, LT.tcc_seq ts')
             end

   in h(tp, d)
  end
 *)

(*
and tycTyc = 
  Stats.doPhase(Stats.makePhase "Compiler 043 1-tycTyc") tycTyc0 x
*)

and tycTyc(fm : flexmap, tc : Types.tycon, d) = 
  let fun dtsTyc nd ({dcons: dconDesc list, arity=i, ...} : dtmember) = 
            let val nnd = if i=0 then nd else DI.next nd
                fun f ({domain=NONE, rep, name}, r) = (LT.tcc_unit)::r
                  | f ({domain=SOME t, rep, name}, r) = 
		    (toTyc fm nnd t)::r

                val _ = enterRecTy i
                val core = LT.tcc_sum(foldr f [] dcons)
                val _ = exitRecTy()

                val resTyc = if i=0 then core
                             else (let val ks = LT.tkc_arg i
                                    in LT.tcc_fn(ks, core)
                                   end)
             in (LT.tkc_int i, resTyc)
            end

      fun dtsFam (freetycs, fam as { members, ... } : dtypeFamily) =
	  case ModulePropLists.dtfLtyc fam of
	      SOME (tc, od) =>
              LT.tc_adj(tc, od, d) (* invariant: tc contains no free variables 
				    * so tc_adj should have no effects *)
	    | NONE => 
              let fun ttk (GENtyc{ arity, ...}) = LT.tkc_int arity
                    | ttk (DEFtyc{tyfun=TYFUN{arity=i, ...},...}) =
		      LT.tkc_int i
                    | ttk _ = bug "unexpected ttk in dtsFam"
                  val ks = map ttk freetycs
                  val (nd, hdr) = 
                      case ks of [] => (d, fn t => t)
                               | _ => (DI.next d, fn t => LT.tcc_fn(ks, t))
                  val mbs = Vector.foldr (op ::) nil members
                  val mtcs = map (dtsTyc (DI.next nd)) mbs
                  val (fks, fts) = ListPair.unzip mtcs
                  val nft = case fts of [x] => x | _ => LT.tcc_seq fts
                  val tc = hdr(LT.tcc_fn(fks, nft)) 
                  val _ = ModulePropLists.setDtfLtyc (fam, SOME(tc, d))
              in tc
              end

(*
      fun dtsFam (_, {lambdatyc=ref (SOME (tc,od)), ...} : dtypeFamily) =
            LT.tc_adj(tc, od, d) (* invariant: tc contains no free variables 
                                    so tc_adj should have no effects *)
        | dtsFam (freetycs, {members, lambdatyc=x, ...}) = 
            let fun ttk (GENtyc { arity, ... }) = LT.tkc_int arity
                  | ttk (DEFtyc{tyfun=TYFUN{arity=i, ...},...}) = LT.tkc_int i
                  | ttk _ = bug "unexpected ttk in dtsFam"
                val ks = map ttk freetycs
                val (nd, hdr) = 
                  case ks of [] => (d, fn t => t)
                           | _ => (DI.next d, fn t => LT.tcc_fn(ks, t))
                val mbs = Vector.foldr (op ::) nil members
                val mtcs = map (dtsTyc (DI.next nd)) mbs
                val (fks, fts) = ListPair.unzip mtcs
                val nft = case fts of [x] => x | _ => LT.tcc_seq fts
                val tc = hdr(LT.tcc_fn(fks, nft)) 
                val _ = (x := SOME(tc, d))
             in tc
            end
*)

      fun h (_,PRIMITIVE pt, _) = LT.tcc_prim (PrimTyc.pt_fromint pt)
        | h (_,DATATYPE {index, family, freetycs, stamps, root}, _) = 
              let val tc = dtsFam (freetycs, family)
                  val n = Vector.length stamps 
                  val names = Vector.map (fn ({tycname,...}: dtmember) => 
					     Symbol.name tycname)
                                         (#members family)
                  (* invariant: n should be the number of family members *)
               in LT.tcc_fix((n, names, tc, (map g freetycs)), index)
              end
        | h (_,ABSTRACT tc, 0) = (g tc) 
              (*>>> LT.tcc_abs(g tc) <<<*) 
        | h (_,ABSTRACT tc, n) = (g tc) 
              (*>>> we tempoarily turned off the use of abstract tycons in
                    the intermediate language; proper support of ML-like
                    abstract types in the IL may require changes to the
                    ML language. (ZHONG)
              let val ks = LT.tkc_arg n
                  fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []
                  val fs = fromto(0, n)
                  val ts = map (fn i => LT.tcc_var(DI.innermost, i)) fs
                  val b = LT.tcc_app(tycTyc(tc, DI.next d), ts)
               in LT.tcc_fn(ks, LT.tcc_abs b)
              end
              <<<*)
        (* | h (TP.FLEXTYC tp, _) = tpsTyc d tp *)
        | h (stmp,FORMAL, n) = 
	  (case FTM.find(fm, stmp)
	    of NONE => (debugmsg ("--tycTyc unable to find "^
				  Stamps.toShortString stmp);
			bug ("unexpected FORMAL kind in tycTyc-h"))
	     | SOME tp => (debugmsg ("--tycTyc found "^
				     Stamps.toShortString stmp);
			   tpsTyc fm d tp))
        | h (_,TEMP, _) = bug "unexpected TEMP kind in tycTyc-h"

      and g (tycon as GENtyc {stamp, arity, kind, ...}) =
	  (case kind of
	       k as DATATYPE _ =>
	       if TU.eqTycon(tycon, BT.refTycon) then LT.tcc_prim(PT.ptc_ref)
	       else h(stamp,k,arity)
	     | k => h(stamp, k, arity))
	| g (DEFtyc{tyfun, ...}) = tfTyc(fm, tyfun, d)
	| g (RECtyc i) = recTyc i
	| g (FREEtyc i) = freeTyc i
	| g (RECORDtyc _) = bug "unexpected RECORDtyc in tycTyc-g"
	| g (PATHtyc{arity, path=InvPath.IPATH ss, entPath}) = 
	  ((* say "*** Warning for compiler writers: PATHtyc ";
	    app (fn x => (say (Symbol.name x); say ".")) ss;
	    say " in translate: ";
	    say (EntPath.entPathToString entPath);
	    say "\n"; *)
	   if arity > 0 then LT.tcc_fn(LT.tkc_arg arity, LT.tcc_void)
	   else LT.tcc_void)
	| g (ERRORtyc) = bug "unexpected tycon in tycTyc-g"
   in g tc
  end

and tfTyc (fm : flexmap, TYFUN{arity=0, body}, d) = toTyc fm d body
  | tfTyc (fm, TYFUN{arity, body}, d) = 
      let val ks = LT.tkc_arg arity
       in LT.tcc_fn(ks, toTyc fm (DI.next d) body)
      end

and toTyc (fm : flexmap) d t = 
  let val m : (tyvar * LT.tyc) list ref = ref []
      fun lookTv tv = 
        let val xxx = !m
            fun uu ((z as (a,x))::r, b, n) = 
                 if a = tv then (x, z::((rev b)@r)) else uu(r, z::b, n+1)
              | uu ([], b, n) = let val zz = h (!tv)
                                    val nb = if n > 64 then tl b else b
                                 in (zz, (tv, zz)::(rev b))
                                end
            val (res, nxx) = uu(xxx, [], 0)
         in m := nxx; res
        end

      and h (INSTANTIATED t) = g t
        | h (LBOUND(SOME{depth,index,...})) =
             LT.tcc_var(DI.calc(d, depth), index)
        | h (UBOUND _) = LT.tcc_void
            (* DBM: should this have been converted to an LBOUND before
             * being passed to toTyc? 
	     * GK: Doesn't seem to experimentally *)
            (* dbm: a user-bound type variable that didn't get generalized;
               treat the same as an uninstantiated unification variable. 
	       E.g. val x = ([]: 'a list; 1) *)
        | h (OPEN _) = LT.tcc_void
            (* dbm: a unification variable that was neither instantiated nor
	       generalized.  E.g. val x = ([],1); -- the unification variable
               introduced by the generic instantiation of the type of [] is
               neither instantiated nor generalized. *)
        | h _ = bug "toTyc:h" (* LITERAL and SCHEME should not occur *)

      and g (VARty tv) = lookTv tv
	| g (CONty(RECORDtyc _, [])) = LT.tcc_unit
        | g (CONty(RECORDtyc _, ts)) = LT.tcc_tuple (map g ts)
        | g (CONty(tyc, [])) = (debugmsg "--toTyc[CONty[]]"; 
				tycTyc(fm, tyc, d))
        | g (CONty(DEFtyc{tyfun,...}, args)) = 
	  (debugmsg "--toTyc[CONty[DEFtyc]"; g(TU.applyTyfun(tyfun, args)))
	| g (CONty (tc as GENtyc { kind, ... }, ts)) =
	  (case (kind, ts) of
	       (ABSTRACT _, ts) =>
	       (debugmsg "--toTyc[CONty[ABSTRACT]]";
		LT.tcc_app(tycTyc(fm, tc, d), map g ts))
             | (_, [t1, t2]) =>
               if TU.eqTycon(tc, BT.arrowTycon) 
	       then LT.tcc_parrow(g t1, g t2)
               else LT.tcc_app(tycTyc(fm, tc, d), [g t1, g t2])
	     | _ => LT.tcc_app (tycTyc (fm, tc, d), map g ts))
        | g (CONty(tyc, ts)) = LT.tcc_app(tycTyc(fm, tyc, d), map g ts)
        | g (IBOUND i) = LT.tcc_var(DI.innermost, i) 
			 (* [KM] IBOUNDs are encountered when toTyc
                          * is called on the body of a POLYty in 
                          * toLty (see below). *)
	| g (MARKty (t, _)) = g t
        | g (POLYty _) = bug "unexpected poly-type in toTyc"
	| g (UNDEFty) = 
          (* mkVB kluge!!! *) LT.tcc_void
	  (* bug "unexpected undef-type in toTyc" *)
        | g (WILDCARDty) = bug "unexpected wildcard-type in toTyc"      

   in g t
  end (* toTyc *)

and toLty (fm : flexmap) d (POLYty {tyfun=TYFUN{arity=0, body}, ...}) = 
    toLty (fm : flexmap) d body
  | toLty (fm : flexmap) d (POLYty {tyfun=TYFUN{arity, body},...}) = 
      let val ks = LT.tkc_arg arity
       in LT.ltc_poly(ks, [toLty fm (DI.next d) body])
      end
  | toLty (fm : flexmap) d  x = LT.ltc_tyc (toTyc fm d x) 

(****************************************************************************
 *               TRANSLATING ML MODULES INTO FLINT TYPES                    *
 ****************************************************************************)

fun specLty (fm : flexmap, elements : (Symbol.symbol * spec) list, entEnv, 
	     depth, compInfo) = 
  let val _ = debugmsg ">>specLty"
      fun g ([], entEnv, ltys) = rev ltys
        | g ((sym, (TYCspec _ ))::rest, entEnv, ltys) =
              (debugmsg ("--specLty[TYCspec] "^Symbol.name sym); 
	       g(rest, entEnv, ltys))
        | g ((sym, STRspec {sign, entVar, ...})::rest, entEnv, ltys) =
              let val rlzn = EE.lookStrEnt(entEnv,entVar)
                  val _ = debugmsg ("--specLty[STRspec] "^Symbol.name sym)
		  val lt = strRlznLty(fm, sign, rlzn, depth, compInfo) 
               in g(rest, entEnv, lt::ltys)
              end
        | g ((sym, FCTspec {sign, entVar, ...})::rest, entEnv, ltys) = 
              let val rlzn = EE.lookFctEnt(entEnv,entVar)
                  val _ = debugmsg ("--specLty[FCTspec] "^Symbol.name sym)
		  val lt = fctRlznLty(fm, sign, rlzn, depth, compInfo) 
               in g(rest, entEnv, lt::ltys)
              end
        | g ((sym, spec)::rest, entEnv, ltys) =
              let val _ = debugmsg ("--specLtyElt "^Symbol.name sym)
                  (* TODO translate entEnv results here? *)
		  fun transty ty = 
                    ((MU.transType entEnv ty)
                      handle EE.Unbound =>
                         (debugmsg "$specLty";
                          withInternals(fn () =>
                           debugPrint("entEnv: ",
                                 (fn pps => fn ee => 
                                  PPModules.ppEntityEnv pps (ee,SE.empty,12)),
                                  entEnv));
                          debugmsg ("$specLty: should have printed entEnv");
                          raise EE.Unbound))

                  fun mapty t = 
		      let val t' = transty t 
			      handle _ => (bug "specLty[mapty,transty]")
		      in toLty fm depth t'  
			 handle _ => bug "specLty[mapty]"
		      end

               in case spec
                   of VALspec{spec=typ,...} => 
                        (debugmsg "--specLty[VALspec]";
			 g(rest, entEnv, (mapty typ)::ltys))
                    | CONspec{spec=DATACON{rep=DA.EXN _, 
                                           typ, ...}, ...} => 
                        let val _ = debugmsg "--specLty[CONspec]\n"
			    val argt = 
                              if BT.isArrowType typ then  
                                   #1(LT.ltd_parrow (mapty typ))
                              else LT.ltc_unit
                         in g(rest, entEnv, (LT.ltc_etag argt)::ltys)
                        end
                    | CONspec{spec=DATACON _, ...} =>
                        (debugmsg "--specLty[CONspec]"; g(rest, entEnv, ltys))
                    | _ => bug "unexpected spec in specLty"
              end
      val res = g (elements, entEnv, [])
      val _  = debugmsg ("<<specLty")
   in res
  end

(*
and signLty (sign, depth, compInfo) = 
  let fun h (SIG {kind=SOME _, lambdaty=ref (SOME(lt, od)), ...}) = lt
             (* LT.lt_adj(lt, od, depth) *)
        | h (sign as SIG{kind=SOME _, lambdaty as ref NONE, ...}) = 
          (* Invariant: we assum that all Named signatures (kind=SOME _) are
           * defined at top-level, outside any functor definitions. (ZHONG)
           *)
             let val {rlzn=rlzn, tycpaths=tycpaths} = 
                   INS.instParam {sign=sign, entEnv=EE.empty, depth=depth,
                                  rpath=InvPath.IPATH[], compInfo=compInfo,
                                  region=SourceMap.nullRegion}
                 val nd = DI.next depth
                 val nlty = strMetaLty(sign, rlzn, nd, compInfo)

                 val ks = map tpsKnd tycpaths
                 val lt = LT.ltc_poly(ks, nlty)
              in lambdaty := SOME (lt, depth); lt
             end
        | h _ = bug "unexpected sign in signLty"
   in h sign
  end
*)
(* sign is paramsig
   rlzn is argRlzn
 *) 
and strMetaLty (fm : flexmap, sign, rlzn as { entities, ... }: strEntity, 
		depth, compInfo, envop) =
    case (sign, ModulePropLists.strEntityLty rlzn) of
	(_, SOME (lt, od)) => LT.lt_adj(lt, od, depth)
      | (SIG { elements, ... }, NONE) => 
	let val entenv' = (case envop 
			    of NONE => entities
			     | SOME env => EE.atop(entities, env))
	    val ltys = specLty (fm, elements, entities, depth, 
				compInfo)
            val lt = (* case ltys of [] => LT.ltc_int
                                   | _ => *) LT.ltc_str(ltys)
        in
	    ModulePropLists.setStrEntityLty (rlzn, SOME(lt, depth));
	    lt
        end
      | _ => bug "unexpected sign and rlzn in strMetaLty"

and strRlznLty (fm : flexmap, sign, rlzn : strEntity, depth, compInfo) =
    case (sign, ModulePropLists.strEntityLty rlzn) of
	(sign, SOME (lt,od)) => LT.lt_adj(lt, od, depth)

(* Note: the code here is designed to improve the "toLty" translation;
   by translating the signature instead of the structure, this can 
   potentially save time on strLty. But it can increase the cost of
   other procedures. Thus we turn it off temporarily. (ZHONG)

      | (SIG{kind=SOME _, ...}, {lambdaty, ...}) =>
             let val sgt = signLty(sign, depth, compInfo)
                 (* Invariant: we assum that all Named signatures 
                  * (kind=SOME _) are defined at top-level, outside any 
                  * functor definitions. (ZHONG)
                  *)
                 val argtycs = INS.getTycPaths{sign=sign, rlzn=rlzn,
                         entEnv=EE.empty, compInfo=compInfo}
                 val lt = LT.lt_inst(sgt, map (tpsTyc depth) argtycs)
              in lambdaty := SOME(lt, depth); lt
             end
*)
      | _ => (debugmsg ">>strRlznLty[strEntityLty NONE]";
	      strMetaLty(fm, sign, rlzn, depth, compInfo, NONE))

and fctRlznLty (fm : flexmap, sign, rlzn, depth, compInfo) = 
    case (sign, ModulePropLists.fctEntityLty rlzn, rlzn) of
	(sign, SOME (lt, od), _) => LT.lt_adj(lt, od, depth)
      | (fs as FSIG{paramsig, bodysig, ...}, _,
         {closureEnv=env, exp=LAMBDA{primaries,...}, ...}) =>
        let val _ = debugmsg ">>fctRlznLty[instParam]"
            val nd = DI.next depth

	    (* [GK 4/30/09] Closure environments map entity paths to 
               types that have no connection to the stamps of the formal 
               types they map. 
               
               [GK 4/24/09] We must somehow account for the closure 
	       environment env. It contains important realization 
	       information such as parameter information from 
               partially applied curried functors. 
             *)

	    val {rlzn=paramRlzn, primaries=_} = 
		INS.instFormal{sign=paramsig,entEnv=env,
			       rpath=InvPath.IPATH[], compInfo=compInfo,
			       region=SourceMap.nullRegion}
	    (* val (tps, ftmap1) = RepTycProps.getTk(fs, paramRlzn, depth)
	    val fm = FTM.unionWith (fn(tp1,tp2)=> tp1) (fm, ftmap1) *)
	    val _ = debugmsg ">>tpsKnd"
	    val ks = map tpsKnd primaries

	    val _ = if !debugging 
		    then (debugmsg "====================";
(*			  withInternals(fn () =>
			    debugPrint("paramRlzn: ",
				       (fn pps => fn ee =>
					PPModules.ppEntity pps (ee,SE.empty,100)),
				       (STRent paramRlzn))); *)
			  withInternals(fn () =>
			    debugPrint("closure: ",
				       (fn pps => fn env =>
					PPModules.ppEntityEnv pps (env,SE.empty,100)),
				       env));
			  debugmsg "====================")
		    else ()

	    val _ = debugmsg ">>strMetaLty"
            val paramLty = strMetaLty(fm, paramsig, primaries, nd, compInfo,
				     SOME env)
		handle _ => bug "fctRlznLty 2"
		     
	    val _ = debugmsg (">>fctRlznLty calling evalApp nd "^DI.dp_print nd)
            val bodyRlzn = 
                EV.evalApp(rlzn, paramRlzn, EPC.initContext,
                           IP.empty, compInfo)
	    val _ = if !debugging
		    then (debugmsg "==================";
			  withInternals(fn () =>
                           debugPrint("evalApp: ",
                                 (fn pps => fn ee => 
                                  PPModules.ppEntity pps (ee,SE.empty,100)),
                                  (STRent bodyRlzn)));
			  withInternals(fn () =>
                           debugPrint("paramRlzn: ",
                                 (fn pps => fn ee => 
                                  PPModules.ppEntity pps (ee,SE.empty,100)),
                                  (STRent paramRlzn)));
			  debugmsg "====================")
		    else ()
	    val _ = debugmsg ">>strRlznLty"
	    (* [GK 5/5/09] Ideally, we want to be able to compute this 
	       without having to appeal to EV.evalApp to get bodyRlzn *)
            val bodyLty = strRlznLty(fm, bodysig, bodyRlzn, nd, compInfo)
	    val _ = debugmsg "<<strRlznLty"
            val lt = LT.ltc_poly(ks, [LT.ltc_fct([paramLty],[bodyLty])])
        in
	    ModulePropLists.setFctEntityLty (rlzn, SOME (lt, depth));
	    debugmsg "<<fctRlznLty";
	    lt
        end 
      | _ => bug "fctRlznLty"

and strLty (fm : flexmap, str as STR { sign, rlzn, ... }, depth, compInfo) =
    (case ModulePropLists.strEntityLty rlzn of
	 SOME (lt, od) => LT.lt_adj(lt, od, depth)
       | NONE =>
         let val _ = debugmsg ">>strLty"
	     val lt = strRlznLty(fm, sign, rlzn, depth, compInfo)
         in
	     ModulePropLists.setStrEntityLty (rlzn, SOME(lt, depth));
	     debugmsg "<<strLty";
	     lt
         end)
  | strLty _ = bug "unexpected structure in strLty"

and fctLty (fm : flexmap, fct as FCT { sign, rlzn, ... }, depth, compInfo) =
    (debugmsg ">>fctLty";
     (case ModulePropLists.fctEntityLty rlzn of
	 SOME (lt,od) => (debugmsg "--fctLty[proplist] "; 
			  LT.lt_adj(lt, od, depth))
       | NONE =>
         let val _ = debugmsg ">>fctLty[computing]"
	     val lt = fctRlznLty(fm, sign, rlzn, depth, compInfo) 
	 in
	     ModulePropLists.setFctEntityLty (rlzn, SOME(lt,depth));
	     debugmsg "<<fctLty";
	     lt
         end))
  | fctLty _ = bug "unexpected functor in fctLty"

(****************************************************************************
 *           A HASH-CONSING VERSION OF THE ABOVE TRANSLATIONS               *
 ****************************************************************************)

(*
structure MIDict = RedBlackMapFn(struct type ord_key = ModuleId.modId
                                     val compare = ModuleId.cmp
                              end)
*)

(*
      val m1 = ref (MIDict.mkDict())   (* modid (tycon) -> LT.tyc *)
      val m2 = ref (MIDict.mkDict())   (* modid (str/fct) -> LT.lty *)

      fun tycTycLook (t as (GENtyc _ | DEFtyc _), d) = 
            let tid = MU.tycId t
             in (case MIDict.peek(!m1, tid)
                  of SOME (t', od) => LT.tc_adj(t', od, d) 
                   | NONE => 
                       let val x = tycTyc (t, d)
                           val _ = (m1 := TcDict.insert(!m1, tid, (x, d)))
                        in x
                       end)
            end
        | tycTycLook x = tycTyc tycTycLook x

(*
      val toTyc = toTyc tycTycLook
      val toLty = toTyc tycTycLook
*)
      val coreDict = (toTyc, toLty)

      fun strLtyLook (s as STR _, d) = 
            let sid = MU.strId s
             in (case MIDict.peek(!m2, sid)
                  of SOME (t', od) => LT.lt_adj(t', od, d)
                   | NONE => 
                       let val x = strLty (coreDict, strLtyLook, 
                                           fctLtyLook) (s, d)
                           val _ = (m2 := TcDict.insert(!m2, sid, (x, d)))
                        in x
                       end)
            end
        | strLtyLook x = strLty (coreDict, strLtyLook, fctLtyLook)

      and fctLtyLook (f as FCT _, d) = 
            let fid = fctId f
             in (case MIDict.peek(!m2, fid)
                  of SOME (t', od) => LT.lt_adj(t', od, d)
                   | NONE => 
                       let val x = fctLty (tycTycLook, strLtyLook, 
                                           fctLtyLook) (s, d)
                           val _ = (m2 := TcDict.insert(!m2, fid, (x, d)))
                        in x
                       end)
            end
        | fctLtyLook x = fctLty (coreDict, strLtyLook, fctLtyLook)
*)

   in {tpsKnd=tpsKnd, tpsTyc=tpsTyc,
       toTyc=toTyc, toLty=toLty, strLty=strLty, fctLty=fctLty}
  end (* function genTT *)

end (* toplevel local *)
end (* structure TransTypes *)
