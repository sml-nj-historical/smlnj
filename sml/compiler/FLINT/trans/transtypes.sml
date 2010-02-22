(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* transtypes.sml *)

signature TRANSTYPES = 
sig

  type primaryEnv = Modules.primary list list
  datatype argtyc 
    = TYCarg of Types.tycon
    | FCTarg of Modules.fctEntity
		
  (* val tpsKnd : tycpath -> PLambdaType.tkind, *)
  val primaryToTyc : argtyc * primaryEnv * int -> PLambdaType.tyc
 
  val tyconToTyc : Types.tycon * primaryEnv * int -> PLambdaType.tyc
  val tyToTyc  : primaryEnv * DebIndex.depth * Types.ty -> PLambdaType.tyc
  val toLty  : primaryEnv -> DebIndex.depth -> Types.ty -> PLambdaType.lty
  val strLty : Modules.Structure * primaryEnv * DebIndex.depth 
               * ElabUtil.compInfo -> PLambdaType.lty
  val fctLty : Modules.Functor * primaryEnv * int
               * ElabUtil.compInfo -> PLambdaType.lty

end (* signature TRANSTYPES *)

structure TransTypes : TRANSTYPES = 
struct
local structure T = Types
      structure BT = BasicTypes
      structure DA = Access   
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

type primaryEnv = Modules.primary list list
(* we may not need the sig and entpath parts. stamp list list may suffice
 * for the translation from primaries to db indexes *)

datatype argtyc 
  = TYCarg of Types.tycon
  | FCTarg of Modules.fctEntity

(*
type primaryEnv = (Types.tycon list 
		   * ((Stamps.stamp * Modules.fctSig) list)) list
*)
(* this is superceded by datatype primary in Instantiate *)
(* datatype primary = FormalTyc of Types.tycon
		 | FormalFct of Stamps.stamp * fctSig
*)

fun bug msg = ErrorMsg.impossible ("TransTypes: " ^ msg)
val say = Control.Print.say 
val debugging = FLINT_Control.tmdebugging (* ref true *)
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

(* this returns a nonnegative int *)
fun relativeDepth (useDepth: int, bindDepth: int) = 
  if bindDepth > useDepth then bug "the definition is deeper than the use"
  (* ASSERT: useDepth >= bindDepth *)
  else (useDepth - bindDepth)


(****************************************************************************
 *               TRANSLATING ML TYPES INTO FLINT TYPES                      *
 ****************************************************************************)
(* a global stack recording entry into dt tycon translations
 * when entering into a dt, push its arity, when finished, pop.
 * Can the arity be passed as a context parameter to tyconToTyc, etc? *)
local val recTyContext : int ref = ref []
in 
fun enterRecTy (a) = (recTyContext := (a::(!recTyContext)))
fun exitRecTy () = (recTyContext := tl (!recTyContext))

fun recTyc (i) = 
    case !recTyContext
     of (x::_) =>
	if x = 0 then LT.tcc_var(1, i)
	   (* no poly abstraction, index to generator abstraction *)
	else if x > 0 then LT.tcc_var(2, i)   
	   (* poly abstraction, so reach beyond poly abstraction to generator abstr *)
	else bug "transtypes: recTyc: negative arity" (* arity should be non-negative *)
      | nil => bug "transtypes: RECtyc outside of a datatype"

fun freeTyc (i) = 
    case !recTyContext
     of (x::_) =>
	if x = 0 then LT.tcc_var(2, i)
	   (* no poly abstraction, reach beyond generator abstraction
	    * to free tyc abstraction *)
	else if x > 0 then LT.tcc_var(3, i)
	   (* poly abstraction, so reach beyond generator and poly abstractions *)
	else bug "transtypes: freeTyc: negative arity"
      | nil => bug "transtypes: FREEtyc outside of a datatype"

end (* end of recTyc and freeTyc hack *)


(* 
(* translating tycpaths to tkinds *)
fun tpsKnd (TP.TP_VAR{kind,...}) = kind 
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
  | tpsKnd _ = bug "unexpected tycpath parameters in tpsKnd"

(* translating tycpath to LT.tyc -- *)
fun tpsTyc (penv : flexmap) d tp = 
  let fun h (TP.TP_VAR {tdepth, num, ...}, cur) =
            let val finaldepth = relativeDepth(cur, tdepth)
		val _ = debugmsg ("--tpsTyc: producing tcc_var "^
				Int.toString tdepth^" "
			   ^Int.toString num^" current depth "^Int.toString cur)
	    in
		if finaldepth < 0 then bug "Invalid depth calculation"
		else LT.tcc_var(finaldepth, num)
	    end
        | h (TP.TP_TYC tc, cur) = tyconToTyc(penv, tc, cur)
        | h (TP.TP_SEL (tp, i), cur) = LT.tcc_proj(h(tp, cur), i)
        | h (TP.TP_APP (tp, ps), cur) = 
              LT.tcc_app(h(tp, cur), map (fn x => h(x, cur)) ps)
        | h (TP.TP_FCT (ps, ts), cur) = 
             let val _ = debugmsg ">>tpsTyc[TP_FCT]"
		 val ks = map tpsKnd ps
                 val cur' = cur + 1
                 val ts' = map (fn x => h(x, cur')) ts
		 val _ = debugmsg "<<tpsTyc[TP_FCT]"
             in LT.tcc_fn(ks, LT.tcc_seq ts')
             end

   in h(tp, d)
  end
*)

(* based on tpsTyc *)
fun primaryToTyc (primary: argtyc, penv : primaryEnv, depth: int) =
    case primary 
     of TYCarg tycon => tyconToTyc(tycon,penv,depth)
      | FCTarg fctEnt => bug "Unimplemented"

(*
and tyconToTyc = 
  Stats.doPhase(Stats.makePhase "Compiler 043 1-tyconToTyc") tyconToTyc x
*)

(* translate Types.tycon to LT.tyc *)
and tyconToTyc(tc : Types.tycon, penv : primaryEnv, depth: int): LT.tyc =
    let fun dtsTyc depth1 ({dcons: dconDesc list, arity, ...} : dtmember) = 
	    let val depth2 = if arity = 0 then depth1 else depth1 + 1
		fun f ({domain=NONE, rep, name}, r) = (LT.tcc_unit)::r
		  | f ({domain=SOME t, rep, name}, r) = 
		    (tyToTyc penv depth2 t)::r

		val _ = enterRecTy arity
		val core = LT.tcc_sum(foldr f [] dcons)
		val _ = exitRecTy()

		(* add the polymorphic abstraction if dt has arity > 0 *)
		val resTyc = if arity=0 then core
			     else (let val ks = LT.tkc_arg arity
				    in LT.tcc_fn(ks, core)
				   end)
	     in (LT.tkc_int arity, resTyc)
	    end

	fun dtsFam (freetycs, fam as { members, ... } : dtypeFamily) =
	    (* removed memoization using ModulePropLists.dtfLtyc fam *)
	    let fun ttk (GENtyc{arity, ...}) = LT.tkc_int arity
		  | ttk (DEFtyc{tyfun=TYFUN{arity, ...},...}) =
		    LT.tkc_int arity
		  | ttk _ = bug "unexpected ttk in dtsFam"
		val ks = map ttk freetycs  (* kinds of free tycon params *)
                (* are there any freetycs?  If so, abstract over them in fix body
		 * tcc *)
		val (depth1, hdr) = 
		    case ks
		     of [] => (depth, fn t => t)
		      | _ => (depth + 1, fn t => LT.tcc_fn(ks, t))
		val mbs = Vector.foldr (op ::) nil members
		val mtcs = map (dtsTyc (depth1 + 1)) mbs
		val (fks, fts) = ListPair.unzip mtcs
		val nft = case fts of [x] => x | _ => LT.tcc_seq fts
		(* add the abstraction for the fix generator, and if needed,
		 * the outer abstraction over the freetycs (done by hdr) *)
		val tc = hdr(LT.tcc_fn(fks, nft)) 
(* memoize	val _ = ModulePropLists.setDtfLtyc (fam, SOME(tc, depth)) *)
	     in tc
	    end

	fun gentyc (_, PRIMITIVE pt) = LT.tcc_prim (PrimTyc.pt_fromint pt)
	  | gentyc (stmp, DATATYPE {index, family, freetycs, stamps, root}) = 
	    if Stamps.eq(stmp, TU.tycStamp(BT.refTycon)) then LT.tcc_prim(PT.ptc_ref) else
	      let val tc = dtsFam (freetycs, family)
		  val n = Vector.length stamps (* dt family size *)
		  val names = Vector.map (fn ({tycname,...}: dtmember) => 
					     Symbol.name tycname)
					 (#members family)
		  (* invariant: n should be the number of family members *)
	       in LT.tcc_fix((n, names, tc, (map toTyc freetycs)), index)
	      end
	  | gentyc (_, ABSTRACT tc) = (toTyc tc)
	  | gentyc (stmp, FORMAL) = 
	      let fun findindex (((_,s1,_)::rest)::penv, index, num) =
		      if Stamps.eq(s1,stmp) then (index, num)
		      else findindex (rest::penv, index, num + 1)
		    | findindex ([]::penv, index, num) = 
		      findindex(penv, index + 1, 0)
		    | findindex ([],_,_) = 
		      bug "formal tycon not bound in primary environment"
		  val (dbIndex, num) = findindex(penv, 1, 0)
	       in LT.tcc_var(dbIndex, num)
	      end
	  | gentyc (_, TEMP) = bug "unexpected TEMP kind in tyconToTyc-h"

	and toTyc (tycon as GENtyc {stamp, arity, kind, ...}): LT.tyc =
	      gentyc(stamp, kind)
	  | toTyc (DEFtyc{tyfun, ...}) = tfTyc(penv, tyfun, d)
	  | toTyc (RECtyc i) = recTyc i
	  | toTyc (FREEtyc i) = freeTyc i
	  | toTyc (PATHtyc{arity, path=InvPath.IPATH ss, entPath}) = (* bug? *)
 	    ((* say "*** Warning for compiler writers: PATHtyc ";
	      app (fn x => (say (Symbol.name x); say ".")) ss;
	      say " in translate: ";
	      say (EntPath.entPathToString entPath);
	      say "\n"; *)
	     (* this should not happen -- pathtycs should have been interpreted
	      * before translation.  Returning a dummy PT.tyc *)
	     if arity > 0 then LT.tcc_fn(LT.tkc_arg arity, LT.tcc_void)
	     else LT.tcc_void)
	  | toTyc (RECORDtyc _) = bug "unexpected RECORDtyc in tyconToTyc-g"
	  | toTyc (ERRORtyc) = bug "unexpected tycon in tyconToTyc-g"
     in toTyc tc
    end (* fun tyconToTyc *)

and tfTyc (penv : primaryEnv, TYFUN{arity=0, body}, depth) : LT.tyc = 
      tyToTyc penv depth body
  | tfTyc (penv, TYFUN{arity, body}, d) = 
      let val ks = LT.tkc_arg arity
       in LT.tcc_fn(ks, tyToTyc ([]::penv) (d+1) body)
      end

(* translating Types.ty to LT.tyc *)
and tyToTyc (penv : primaryEnv, d: int, t: T.ty) : LT.tyc = 
  let val tvmemo : (tyvar * LT.tyc) list ref = ref []
      fun lookTv tv = 
          let fun uu ((z as (a,x))::r, b, n) = 
                  if a = tv then (x, z::((rev b)@r)) else uu(r, z::b, n+1)
		| uu ([], b, n) = let val zz = tyvar (!tv)
                                      val nb = if n > 64 then tl b else b
                                   in tvmemo := (tv, zz)::(rev nb);
				      zz
                                  end
           in uu(!tvmemo, [], 0)
          end

      and tyvar (INSTANTIATED t) = typ t
        | tyvar (LBOUND(SOME{depth,index,...})) =
             LT.tcc_var(relativeDepth(d, depth), index)
        | tyvar (UBOUND _) = LT.tcc_void
            (* DBM: should this have been converted to an LBOUND before
             * being passed to tyToTyc? 
	     * GK: Doesn't seem to experimentally *)
            (* dbm: a user-bound type variable that didn't get generalized;
               treat the same as an uninstantiated unification variable. 
	       E.g. val x = ([]: 'a list; 1) *)
        | tyvar (OPEN _) = LT.tcc_void
            (* dbm: a unification variable that was neither instantiated nor
	       generalized.  E.g. val x = ([],1); -- the unification variable
               introduced by the generic instantiation of the type of [] is
               neither instantiated nor generalized. *)
        | tyvar _ = bug "tyToTyc:h" (* LITERAL and SCHEME should not occur *)

      and typ (VARty tv) = lookTv tv
	| typ (CONty(RECORDtyc _, [])) = LT.tcc_unit
        | typ (CONty(RECORDtyc _, ts)) = LT.tcc_tuple (map typ ts)
        | typ (CONty(tyc, [])) = (debugmsg "--tyToTyc[CONty[]]"; 
				  tyconToTyc(tyc, penv, d))
        | typ (CONty(DEFtyc{tyfun,...}, args)) = 
	  (debugmsg "--tyToTyc[CONty[DEFtyc]";
	   typ (TU.applyTyfun(tyfun, args)))
	| typ (CONty (tc as GENtyc { kind, ... }, ts)) =
	  (case (kind, ts)
	     of (ABSTRACT _, ts) =>
	        (debugmsg "--tyToTyc[CONty[ABSTRACT]]";
		 LT.tcc_app(tyconToTyc(tc, penv, d), map typ ts))
              | (_, [t1, t2]) =>
                 if TU.eqTycon(tc, BT.arrowTycon) 
	         then LT.tcc_parrow(typ t1, typ t2)
                 else LT.tcc_app(tyconToTyc(tc, penv, d), [typ t1, typ t2])
	     | _ => LT.tcc_app (tyconToTyc (tc, penv, d), map typ ts))
        | typ (CONty(tc, ts)) = LT.tcc_app(tyconToTyc(tc, penv, d), map typ ts)
        | typ (IBOUND i) = LT.tcc_var(1, i) 
			 (* [KM] IBOUNDs are encountered when tyToTyc
                          * is called on the body of a TYFUN in 
                          * tfTyc or on a POLYty in Lty (see below). *)
	| typ (MARKty (t, _)) = typ t
        | typ (POLYty _) = bug "unexpected poly-type in tyToTyc"
	| typ (UNDEFty) = LT.tcc_void (* mkVB kluge!!! *) 
	    (* bug "unexpected undef-type in tyToTyc" *)
        | typ (WILDCARDty) = bug "unexpected wildcard-type in tyToTyc"

   in typ t 
  end (* tyToTyc *)

(* translating polytypes *)
and toLty (penv : primaryEnv) d (POLYty {tyfun=TYFUN{arity=0, body}, ...}): LT.lty = 
    toLty (penv : primaryEnv) d body  (* degenerate polytype *)
  | toLty (penv : primaryEnv) d (POLYty {tyfun=TYFUN{arity, body},...}) = 
      let val ks = LT.tkc_arg arity
       in LT.ltc_poly(ks, [toLty ([]::penv) (d+1) body])
      end
  | toLty (penv : primaryEnv) d  x = LT.ltc_tyc (tyToTyc(penv, d, x))


(****************************************************************************
 *               TRANSLATING ML MODULES INTO FLINT TYPES                    *
 ****************************************************************************)

(* specLty: return lty for the dynamic elements of a structure (i.e.
   structure, functor, value, and exn constr elements. The lty describes
   the dynamic aspect of the element.
   The entEnv is assumed to be from the realization associated with the
   signature elements in some structure fullsig. *)
fun specLty (elements : (Symbol.symbol * spec) list, entEnv : EE.entityEnv, 
	     penv : primaryEnv, depth: int, compInfo) : LT.lty list = 
  let val _ = debugmsg ">>specLty"
      (* elemsLty : M.elements -> LT.lty list *)
      fun elemsLty ([], ltys) = rev ltys
        | elemsLty ((sym, (TYCspec _ ))::rest, entEnv, ltys) =
            (* tycon element, no dynamic value *)
            (debugmsg ("--specLty[TYCspec] "^Symbol.name sym); 
	     elemsLty(rest, ltys))
        | elemsLty ((sym, STRspec {sign, entVar, ...})::rest, ltys) =
            let val rlzn = EE.lookStrEnt(entEnv,entVar)
                val _ = debugmsg ("--specLty[STRspec] "^Symbol.name sym)
		val lt = strLty0(sign, rlzn, penv, depth, compInfo, NONE)
             in elemsLty(rest, lt::ltys)
            end
        | elemsLty ((sym, FCTspec {sign, entVar, ...})::rest, ltys) = 
            let val rlzn = EE.lookFctEnt(entEnv,entVar)
                val _ = debugmsg ("--specLty[FCTspec] "^Symbol.name sym)
		val lt = fctLty0(sign, rlzn, penv, depth, compInfo) 
             in elemsLty(rest, lt::ltys)
            end
        | elemsLty ((sym, spec)::rest, ltys) =
	    (* VAL or CON spec *)
	    let val _ = debugmsg ("--specLtyElt "^Symbol.name sym)
		(* TODO translate entEnv results here? *)
		fun instTy ty =   (* instantiate relativized ty wrt entEnv *)
		    (MU.transType entEnv ty)
		     handle EE.Unbound =>
		       (debugmsg "$specLty";
			withInternals(fn () =>
			 debugPrint("entEnv: ",
			       (fn pps => fn ee => 
				PPModules.ppEntityEnv pps (ee,SE.empty,12)),
				entEnv));
			debugmsg ("$specLty: should have printed entEnv");
			raise EE.Unbound)

		fun transTy t = 
		    let val t' = instTy t 
				 handle _ => (bug "specLty[mapty,transty]")
		    in toLty penv depth t'  
		       handle _ => bug "specLty[mapty]"
		    end

	     in case spec
		 of VALspec{spec=typ,...} => 
		      (debugmsg "--specLty[VALspec]";
		       elemsLty (rest, (transTy typ)::ltys))
		  | CONspec{spec=DATACON{rep=DA.EXN _, 
					 typ, ...}, ...} => 
		      let val _ = debugmsg "--specLty[CONspec]\n"
			  val argt = 
			    if BT.isArrowType typ then  
				 #1(LT.ltd_parrow (transTy typ))
			    else LT.ltc_unit
		       in elemsLty (rest, (LT.ltc_etag argt)::ltys)
		      end
		  | CONspec{spec=DATACON _, ...} =>
		      (debugmsg "--specLty[CONspec]";
		       elemsLty (rest, ltys))
		  | _ => bug "unexpected spec in specLty"
	    end
      val res = elemsLty (elements, [])
   in debugmsg ("<<specLty");
      res
  end

(* strLty0 computes Lty describing dynamic content of a structure.
   envop: entityEnv option -- optional external context entityEnv.
   envop will be SOME closureEnv when calculating lty of formal instantiation
   of functor param in fctLty0.
 *) 
and strLty0 (sign, rlzn as { entities, ... }: strEntity, 
	     penv : primaryEnv, depth, compInfo, entEnvOp) =
    case ModulePropLists.strEntityLty rlzn (* check for momoized lty *)
      of SOME (lty, od) =>
	 (* memo exists, adjust stored lty for changed depth (???) *)
	 LT.lt_adj(lty, od, depth)
       | NONE =>    (* no memo, must compute *)
	 (case sig
	   of SIG { elements, ... } =>
	      let val entenv' =
		      case entEnvOp  (* add context entityEnv if provided *)
		        of NONE => entities
			 | SOME entenv => EE.atop(entities, entenv)
		  val ltys = specLty (elements, entities, penv, depth, compInfo)
		  val lty = LT.ltc_str(ltys)  (* bundle ltys of dyn. elements *)
	      in ModulePropLists.setStrEntityLty (rlzn, SOME(lty, depth)); (* memoize *)
		 lty
	      end
	    | _ => bug "strLty0 - unexpected sign")

(* fctLty0 calculates lty for dynamic content of a functor *)
and fctLty0 (fctSign: M.fctSig, fctRlzn: M.fctEntity,
	     penv : primaryEnv, depth: int, compInfo) = 
    case ModulePropLists.fctEntityLty rlzn   (* check for memoization *)
      of SOME (lt, od) => LT.lt_adj(lt, od, depth)  (* use memo *)
       | NONE =>  (* no memo, compute it *)
        let val FSIG{paramsig, bodysig, ...} = fctSign
	    val {closureEnv, primaries, paramEnv, ...} = fctRlzn
            (* we're going to discard primaries and paramEnv from fctRlzn
	     * and recompute primaries (primaries') and paramRlzn by
	     * reinstantiating the paramsig below *)

	    val _ = debugmsg ">>fctLty0[instParam]"
            val nd = depth + 1

	    (* [GK 4/30/09] Closure environments map entity paths to 
               types that have no connection to the stamps of the formal 
               types they map. 
               
               [GK 4/24/09] We must somehow account for the closure 
	       environment closureEnv. It contains important realization 
	       information such as parameter information from 
               partially applied curried functors. 
             *)

            (* [DBM, 5/19/09] here we could use the primaries stored
             * in the fct realization fctRlzn, but we need the
	     * original instantiation realization for the paramsig created
	     * during elaboration to calculate the kind for the parameter.
	     * 
	     * Alternatively, we can re-instantiate paramsig and use the
	     * new primaries and rlzn to calculate the kinds.
	     * We also need to reuse the parameter instantiation for the call
	     * of evalApp below. Whichever instantiation is used to calculate
	     * the penv_layer must also be used for evalApp so that the
	     * type variable bindings (determined via stamps) are correlated
	     * with their occurrences in the body rlzn after evalApp.
	     *)

            (* Do we really need to instantiate paramsig again, given that
	     * we have the saved paramEnv in fctRlzn?  We could recreate a paramRlzn
	     * from paramEnv, or maybe evalApp should just require an
	     * entityEnv for the argument structure instead of a realization?
	     *)
	    val {rlzn=paramRlzn, primaries=primaries'} = 
		INS.instFormal{sign=paramsig,entEnv=closureEnv,
			       rpath=InvPath.IPATH[], compInfo=compInfo,
			       region=SourceMap.nullRegion}
           (* ASSERT: primaries' is isomorphic to primaries, modulo the
	    * change in fresh stamps.
	    * Same for #entities(paramRlzn) vs paramEnv. *)

	    val _ = debugmsg ">>parameter kinds"
         
            (* calculate kinds of parameter's primaries -- 
	     * this is here done with using the primaries and paramEnv from
	     * fctRlzn, but equivalently we would use primaries' and the
	     * entities field of paramRlzn computed by the local instantiation *)
	    val paramPrimaryKinds =
		map (#2 o (FctKind.primaryToBind (compInfo, paramEnv)))
		    primaries
	    (* or, if we used the freshly instantiated versions --
	        map (#2 o (FctKind.primaryToBind (compInfo, #entities(paramRlzn))))
	            primaries'
	     *)

	    val _ = if !debugging 
		    then (debugmsg "====================";
(*			  withInternals(fn () =>
			    debugPrint("paramRlzn: ",
				       (fn pps => fn ee =>
					PPModules.ppEntity pps (ee,SE.empty,100)),
				       (STRent paramRlzn))); *)
			  withInternals(fn () =>
			    debugPrint("closureEnv: ",
				       (fn pps => fn env =>
					PPModules.ppEntityEnv pps (env,SE.empty,100)),
				       closureEnv));
			  debugmsg "====================")
		    else ()

            (* the (SOME closureEnv) arg is passed because the paramsig was
	     * originally relativized wrt that same entityEnv when it was
	     * elaborated, so relative entpaths will need to be interpreted
	     * with respect to closureEnv augmented with local entities. *)
	    val _ = debugmsg ">>strLty0"
            val paramLty = strLty0(paramsig, paramRlzn, penv, nd, compInfo,
				   SOME closureEnv)
		handle _ => bug "fctLty0 2"
		     
	    val _ = debugmsg (">>fctLty0 calling evalApp nd "^Int.toString nd)

            (* use evalApp to propagate parameter elements and generate new body
	     * elements.  If we wanted to use the paramEnv stored in the fctRlzn,
	     * we would need to build a dummy rlzn around it.  Or the fctRlzn
	     * could have stored the entire param sig realization used when the
	     * functor was elaborated. *)
            val bodyRlzn = 
                EV.evalApp(fctRlzn, paramRlzn, EPC.initContext,
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
	    val _ = debugmsg ">>strLty0: functor body"
	    (* [GK 5/5/09] Ideally, we want to be able to compute this 
	       without having to appeal to EV.evalApp to get bodyRlzn.
	       [DBM 5/19/09] This may be possible, but it would require
	       "decompiling" the LAMBDA expression in the functor rlzn. *)

	    (* add "bindings" for the primaries from the parameter
	     * instantiation, so that references to them in the body str can be
	     * translated into type variables. 
	     * Have to use the primaries' version produced by the local
	     * instantiation of the paramsig so that the stamps will match
	     * those propagated by the functor application that computed bodyRlzn. *)
	    val innerPenv = primaries' :: penv

            (* Is bodysig also relatized wrt closureEnv?  If so, (SOME closureEnv)
	     * should be used instead of NONE for the last parameter, just as for
	     * the calculation of paramLty earlier. Or perhaps bodyRlzn already
	     * incorporates closureEnv if necessary? *)
	    val bodyLty = strLty0(bodysig, bodyRlzn, innerPenv, nd, compInfo, NONE)

	    val _ = debugmsg "<<strLty0: functor body"

            (* paramLty and bodyLty are value types, and hence will be of kind M *)
            val lty = LT.ltc_poly(paramPrimaryKinds, [LT.ltc_fct([paramLty],[bodyLty])])
        in
	    ModulePropLists.setFctEntityLty (rlzn, SOME (lty, depth));  (* memoize *)
	    debugmsg "<<fctLty0";
	    lty
        end 
      | _ => bug "fctLty0"

(* strLty: M.Structure * M.primaryEnv * int * compInfo -> LT.lty 
 * essentially just calling strLty0, but with Structure rather than
 * sign, rlzn separately *)
and strLty (str as STR { sign, rlzn, ... }, penv : primaryEnv, depth, compInfo) =
    let val _ = debugmsg ">>strLty"
	val lty = strLty0(sign, rlzn, penv, depth, compInfo, NONE)
     in debugmsg "<<strLty";
	lty
    end
  | strLty _ = bug "unexpected structure in strLty"

(* fctLty: M.functor * M.primaryEnv * int * compInfo -> LT.lty 
 * essentially just calling fctLty0, but with Functor rather than
 * sign, rlzn separately *)
and fctLty (fct as FCT { sign, rlzn, ... }, penv : primaryEnv, depth, compInfo) =
    let val _ = debugmsg ">>fctLty"
	val lty = fctLty0(sign, rlzn, penv, depth, compInfo) 
     in debugmsg "<<fctLty";
	lty
    end
  | fctLty _ = bug "unexpected functor in fctLty"

end (* toplevel local *)
end (* structure TransTypes *)




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
                       let val x = tyconToTyc (t, penv, d)
                           val _ = (m1 := TcDict.insert(!m1, tid, (x, d)))
                        in x
                       end)
            end
        | tycTycLook x = tyconToTyc tycTycLook x

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

