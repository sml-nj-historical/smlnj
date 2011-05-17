(* COPYRIGHT (c) 1995 AT&T Bell Laboratories *)
(* tempexpn.sml *)

structure TemplateExpansion =
struct

local open Types VarCon Access Absyn ErrorMsg MCCommon BasicTypes

in 

exception Lookup

fun lookup (a as VALvar{access=LVAR a',...}, 
		(VALvar{access=LVAR b,...},c)::d) = 
       if a' = b then c else lookup(a, d) 
  | lookup (VALvar _, (VALvar _, _)::_) =
	  ErrorMsg.impossible "833 in tempexpn"
  | lookup _ = raise Lookup

val mkLvar = LambdaVar.mkLvar

exception CANT_MATCH

fun foo x = impossible "no templates yet"
(*
   	(case lookup (x, !constructor_env)
          of {rep = TEMPLrep (NOpat, _, _),...} => raise CANT_MATCH 
           | {rep = TEMPLrep x,...} => x 
           | _ => raise Internal 1)
	handle Lookup => raise (Internal 2) 
*)

fun foo' x = impossible "no symbolic constants yet"
(*
   	(case lookup (x, !constructor_env)
          of {rep = CONSTrep (NOpat, _),...} => raise CANT_MATCH 
           | {rep = CONSTrep x,...} => x 
           | _ => raise Internal 3)
	handle Lookup => raise (Internal 4)
*)

fun andPatterns(WILDpat, pat) = pat
  | andPatterns(pat, WILDpat) = pat
  | andPatterns(CONSTRAINTpat(pat, _), pat') = andPatterns(pat, pat')
  | andPatterns(pat, CONSTRAINTpat(pat', _))= andPatterns(pat, pat')
  | andPatterns(VARpat v, pat) = LAYEREDpat(VARpat v, pat)
  | andPatterns(pat, VARpat v) = LAYEREDpat(VARpat v, pat)
  | andPatterns(CONpat(k,t), CONpat(k',t')) = 
	  if conEq (k, k') then CONpat(k,t)
	  else if abstract k then LAYEREDpat(CONpat(k,t), CONpat(k',t'))
      else if abstract k' then LAYEREDpat(CONpat(k',t'), CONpat(k,t))
      else raise CANT_MATCH
  | andPatterns(CONpat(k,t), APPpat(k',t',pat)) =
	  if abstract k then LAYEREDpat(CONpat(k,t), APPpat(k',t',pat))
      else if abstract k' then LAYEREDpat(APPpat(k',t',pat), CONpat(k,t))
      else raise CANT_MATCH
  | andPatterns(APPpat(k',t',pat), CONpat(k,t)) =
	  if abstract k then LAYEREDpat(CONpat(k,t), APPpat(k',t',pat))
      else if abstract k' then LAYEREDpat(APPpat(k',t',pat), CONpat(k,t))
      else raise CANT_MATCH
  | andPatterns(APPpat(k,t,pat), APPpat(k',t',pat')) =
	  if conEq (k, k') then APPpat(k,t,andPatterns(pat,pat'))
	  else if abstract k then 
                LAYEREDpat(APPpat(k,t,pat),APPpat(k',t',pat'))
      else if abstract k' then 
                LAYEREDpat(APPpat(k',t',pat'), APPpat(k,t,pat))
      else raise CANT_MATCH
  | andPatterns(CONpat(k,t), pat) =
	  if abstract k then LAYEREDpat(CONpat(k,t), pat)
      else impossible "Non abstract CONpat & non constructor pat in andPat"
  | andPatterns(pat, CONpat(k,t)) =
	  if abstract k then LAYEREDpat(CONpat(k,t), pat)
      else impossible "non constructor pat & Non abstract CONpat in andPat"
  | andPatterns(APPpat(k,t,pat), pat') =
	  if abstract k then LAYEREDpat(APPpat(k,t,pat), pat')
      else impossible "Non abstract APPpat & non constructor pat in andPat"
  | andPatterns(pat, APPpat(k,t,pat')) = 
	  if abstract k then LAYEREDpat(APPpat(k,t,pat'), pat)
      else impossible "non constructor pat & Non abstract APPpat in andPat"
  | andPatterns(LAYEREDpat(CONSTRAINTpat(pat1, _), pat2), pat) =
      andPatterns(LAYEREDpat(pat1, pat2), pat) 
  | andPatterns(pat, LAYEREDpat(CONSTRAINTpat(pat1, _), pat2)) =
      andPatterns(pat, LAYEREDpat(pat1, pat2)) 
  | andPatterns(LAYEREDpat(pat1, pat2), pat) =
      LAYEREDpat(pat1, andPatterns(pat2, pat))
  | andPatterns(pat, LAYEREDpat(pat1, pat2)) =
      LAYEREDpat(pat1, andPatterns(pat2, pat))
  | andPatterns(INTpat (p as (s,t)), INTpat (s',t')) =
	  ((if TypesUtil.equalType(t,intTy) then
	        if (LiteralToNum.int s) = (LiteralToNum.int s')
		  then INTpat p
	          else raise CANT_MATCH
        else if TypesUtil.equalType(t,int32Ty) then
	        if (LiteralToNum.int32 s) = (LiteralToNum.int32 s')
		  then INTpat p
	          else raise CANT_MATCH
	    else ErrorMsg.impossible "andPatterns/INTpat in tempexpn")
	   handle Overflow => 
       ErrorMsg.impossible "overflow during int or word patter comparisons")
  | andPatterns(WORDpat (p as (w,t)), WORDpat (w',t')) =
	  ((if TypesUtil.equalType(t,wordTy) then
	        if (LiteralToNum.word w) = (LiteralToNum.word w')
		  then WORDpat p
	          else raise CANT_MATCH
	    else if TypesUtil.equalType(t,word8Ty) then
	        if (LiteralToNum.word8 w) = (LiteralToNum.word8 w')
		  then WORDpat p
	          else raise CANT_MATCH
	    else if TypesUtil.equalType(t,word32Ty) then
	        if (LiteralToNum.word32 w) = (LiteralToNum.word32 w')
		  then WORDpat p
	          else raise CANT_MATCH
	    else ErrorMsg.impossible "andPatterns/WORDpat in tempexpn")
	   handle Overflow => 
       ErrorMsg.impossible "overflow during int or word patter comparisons")
  | andPatterns(REALpat r, REALpat r') = 
	  if r = r' then REALpat r else raise CANT_MATCH
  | andPatterns(STRINGpat s, STRINGpat s') =
	  if s = s' then STRINGpat s else raise CANT_MATCH
  | andPatterns(CHARpat s, CHARpat s') =
	  if s = s' then CHARpat s else raise CANT_MATCH
  | andPatterns(pat1 as RECORDpat{fields=p,...}, 
                pat2 as RECORDpat{fields=q,...}) =
      mkRECORDpat pat1 (multiAnd(map #2 p, map #2 q))

(******************* how to and two types ? **************************)
  | andPatterns(VECTORpat(p,t), VECTORpat(p',t')) =
      if (length p) = (length p') then VECTORpat(multiAnd(p,p'),t) 
      else raise CANT_MATCH
  | andPatterns (p1, p2) = 
	  impossible "bas andPattern call"

and multiAnd (nil, nil) = nil
  | multiAnd (pat::rest, pat'::rest') = 
      (andPatterns(pat,pat'))::(multiAnd(rest, rest'))
  | multiAnd _ = impossible "bad multiAnd call"

fun instantiatePatexp (VARpat v, env) = lookup(v, env)
  | instantiatePatexp (LAYEREDpat(pat1, pat2),env) =
      andPatterns(instantiatePatexp(pat1,env),instantiatePatexp(pat2,env))
  | instantiatePatexp (CONSTRAINTpat(pat, _), env) =
      instantiatePatexp(pat, env)
  | instantiatePatexp (APPpat(k,t,pat), env) = 
	  APPpat(k,t,instantiatePatexp(pat, env))
  | instantiatePatexp (pat as RECORDpat{fields,...}, env) =
      mkRECORDpat pat (multiInstantiatePatexp(map #2 fields, env))
  | instantiatePatexp (VECTORpat(pats,t), env) =
      VECTORpat (multiInstantiatePatexp(pats, env), t)
  | instantiatePatexp (pat, env) = pat
and multiInstantiatePatexp(nil, env) = nil
  | multiInstantiatePatexp(pat::rest, env) = 
	  (instantiatePatexp(pat, env))::(multiInstantiatePatexp(rest, env))

fun instance (VARpat (VALvar {path, typ, prim, btvs, ...})) =
      VARsimp (VALvar{access=LVAR (mkLvar()),
      	path=path, btvs = btvs, typ=typ, prim=prim})
  | instance (VARpat _) = impossible "bad variabel in match"
  | instance (RECORDpat{fields,...}) = 
	       RECORDsimp(map (fn(lab,pat)=>(lab,instance pat)) fields)
  | instance (CONSTRAINTpat(pat, _)) = instance pat
  | instance pat = impossible "bad instance call"

fun simpToPat (VARsimp v) = VARpat v
  | simpToPat (RECORDsimp labsimps) = 
      RECORDpat {fields=map(fn(lab,simp)=>(lab,simpToPat simp)) labsimps,
		     flex=false, typ = ref UNDEFty}

fun trivpatTrivEnv (VARpat v, VARsimp x) = [(v, VARpat x)]
  | trivpatTrivEnv (CONSTRAINTpat(tpat, _), simp) = 
      trivpatTrivEnv (tpat, simp)
  | trivpatTrivEnv (RECORDpat{fields,...}, RECORDsimp labsimps) =
      multiTrivpatTrivEnv (map #2 fields, map #2 labsimps)
  | trivpatTrivEnv _ = impossible "trivpatTrivEnv"
and multiTrivpatTrivEnv (nil, nil) = nil
  | multiTrivpatTrivEnv (tpat::trest, simp::srest)=
      (trivpatTrivEnv(tpat, simp))@(multiTrivpatTrivEnv(trest, srest))
  | multiTrivpatTrivEnv _ = impossible "multiTrivpatTrivEnv"

fun wildEnv (VARpat v) = [(v, WILDpat)]
  | wildEnv (CONSTRAINTpat(tpat, _)) = wildEnv tpat
  | wildEnv (RECORDpat{fields,...}) = List.concat (map (wildEnv o #2) fields)
  | wildEnv _ = impossible "wildEnv called on non-trivpat"

fun matchTrivpat (VARpat v, pat)= ([(v, pat)], nil, nil)
  | matchTrivpat (CONSTRAINTpat(tpat, _), pat) = matchTrivpat(tpat, pat)
  | matchTrivpat (tpat, CONSTRAINTpat(pat, _)) = matchTrivpat(tpat, pat)
  | matchTrivpat (RECORDpat{fields=tps,...},RECORDpat{fields=ps,...}) =
	  multiMatchTrivpat(map #2 tps, map #2 ps)
  | matchTrivpat (tpat, WILDpat) = 
      (wildEnv tpat, nil, nil)
  | matchTrivpat (tpat, VARpat v) =
      let val a = instance tpat
          val b = trivpatTrivEnv (tpat, a)
      in (b, [(v, a)], nil)
      end
  | matchTrivpat (tpat, CONpat(k,t)) =
      let val a = instance tpat
          val b = trivpatTrivEnv (tpat, a)
      in (b, nil, [(a, CONpat(k,t))])
      end
  | matchTrivpat (tpat, APPpat(k,t,pat)) =
      let val a = instance tpat
          val b = trivpatTrivEnv (tpat, a)
      in (b, nil, [(a, APPpat(k,t,pat))])
      end
  | matchTrivpat (tpat, LAYEREDpat(CONpat(k,t), pat)) =
      let val a = instance tpat
          val (pat', varEnv, constr) = 
                matchTrivpat(tpat, andPatterns(simpToPat a, pat))
      in (pat', varEnv, (a, CONpat(k,t))::constr)
      end
  | matchTrivpat (tpat, LAYEREDpat(APPpat(k,t,spat), pat)) =
      let val a = instance tpat
          val (pat', varEnv, constr) = 
                matchTrivpat(tpat, andPatterns(simpToPat a, pat))
      in (pat', varEnv, (a, APPpat(k,t,spat))::constr)
      end
  | matchTrivpat (tpat, LAYEREDpat(VARpat v, pat)) =
      let val a = instance tpat
          val (pat', varEnv, constr) = 
             matchTrivpat(tpat, andPatterns(simpToPat a, pat))
      in (pat', (v,a)::varEnv, constr)
      end
  | matchTrivpat (tpat, LAYEREDpat(CONSTRAINTpat(pat1, _), pat2)) =
      matchTrivpat (tpat, LAYEREDpat(pat1, pat2))
  | matchTrivpat (tpat, pat) = impossible "bad matchTrivpat call"
and multiMatchTrivpat (nil, nil) = (nil, nil, nil)
  | multiMatchTrivpat (tpat::trest, pat::prest) =
	  let val (patenv, varenv, constr) = multiMatchTrivpat(trest, prest)
          val (patenv', varenv', constr') = matchTrivpat(tpat, pat)
      in (patenv@patenv', varenv@varenv', constr@constr')
      end
  | multiMatchTrivpat _ = impossible "bad multiMatchTrivpat call"

fun newVars (RECORDsimp labsimps, env) = 
	  multiNewVars(map #2 labsimps, env)
  | newVars (VARsimp (v as VALvar {path, typ, btvs, prim, ...}), env) =
	  ((lookup(v, env); env) 
         handle Lookup => 
           ((v,VALvar{path=path, typ=typ,access=LVAR (mkLvar()),
                      btvs=btvs,prim=prim})::env))
  | newVars (VARsimp _, _) = impossible "bad instance call to newVars"
and multiNewVars(nil, env) = env
  | multiNewVars(simp::rest, env) = multiNewVars(rest, newVars(simp, env))

fun instantiateLocalVars (nil, env) = env
  | instantiateLocalVars ((path,pat)::rest, env) =
      instantiateLocalVars(rest, newVars(path, env))

fun instSimpexp(VARsimp v, env) = VARsimp (lookup(v, env))
  | instSimpexp(RECORDsimp labsimps, env) = 
      RECORDsimp (multiInstSimpexp (labsimps, env))
and multiInstSimpexp(nil, env) = nil
  | multiInstSimpexp((lab,simpexp)::rest, env) = 
      (lab, instSimpexp(simpexp, env))::(multiInstSimpexp(rest, env))

fun instantiateConstrs(nil, locEnv, env) = nil
  | instantiateConstrs((simpexp, pat)::rest, locEnv, env) = 
      (instSimpexp(simpexp, locEnv), instantiatePatexp(pat, env))
        :: (instantiateConstrs(rest, locEnv, env))    

fun liftenv nil = nil
  | liftenv ((v,x)::rest) = (v, VARpat x)::(liftenv rest)

fun templExpand(k, pat) =
      let 
        val (patexp, trivpat, constrs) = foo k
        val (env, varnames, newconstrs) = matchTrivpat(trivpat, pat)
        val env' = instantiateLocalVars (constrs, nil)
        val newEnv = env@(liftenv env')
      in 
        (instantiatePatexp(patexp, newEnv),
         newconstrs@(instantiateConstrs(constrs, env', newEnv)),
         varnames)
      end              

fun constExpand k =
      let 
        val (patexp, constrs) = foo' k
        val newEnv = instantiateLocalVars (constrs, nil)
	    val lNewEnv = liftenv newEnv
      in 
        (instantiatePatexp(patexp, lNewEnv),
         instantiateConstrs(constrs, newEnv, lNewEnv),
         nil)
      end              

fun multiTemplateExpand nil = (nil, nil, nil)
  | multiTemplateExpand (pat::rest) =
      let 
        val (pats', constr1, varenv1) = multiTemplateExpand rest
        val (pat', constr2, varenv2) = templateExpandPattern pat
      in
        (pat'::pats', constr1@constr2, varenv1@varenv2)
      end

and templateExpandPattern (APPpat(k,t,pat)) =
      let
        val (pat', patConstraints, patVarenv) = templateExpandPattern pat
      in
        if template k then
          let
            val (newPat, kConstraints, kVarenv) = templExpand(k, pat')
          in
            (newPat, patConstraints@kConstraints, patVarenv@kVarenv)
          end
        else
          (APPpat(k,t,pat'), patConstraints, patVarenv)
      end
  | templateExpandPattern (CONpat(k,t)) =
      if template k then
        let
          val (newPat, constraints, varenv) = constExpand k
        in
          (newPat, constraints, varenv)
        end
      else
        (CONpat(k,t), nil, nil)
  | templateExpandPattern (pat as RECORDpat{fields,...}) =
      let 
        val (pats', constr, varenv) = multiTemplateExpand (map #2 fields)
      in
        (mkRECORDpat pat pats', constr, varenv)
      end
  | templateExpandPattern (VECTORpat(pats,t)) =
      let 
        val (pats', constr, varenv) = multiTemplateExpand pats
      in
        (VECTORpat(pats,t), constr, varenv)
      end
  | templateExpandPattern (LAYEREDpat(pat1, pat2)) =
      let 
        val (pat1', constr1, varenv1) = templateExpandPattern pat1
        val (pat2', constr2, varenv2) = templateExpandPattern pat2
      in
        (LAYEREDpat(pat1', pat2'), constr1@constr2, varenv1@varenv2)
      end
  | templateExpandPattern (CONSTRAINTpat(pat, _)) =
      templateExpandPattern pat
  | templateExpandPattern pat = (pat, nil, nil)

fun fullyExpandBinding varenv (VARsimp v) =
      (fullyExpandBinding varenv (lookup(v, varenv))
         handle Lookup => VARsimp v)
  | fullyExpandBinding varenv (RECORDsimp labsimps) =
      RECORDsimp 
	    (map (fn(lab,simp)=>(lab,fullyExpandBinding varenv simp)) labsimps)

fun fullyExpandBindingTrivpat varenv (VARpat v) =
      (fullyExpandBindingTrivpat varenv (simpToPat(lookup(v, varenv)))
         handle Lookup => VARpat v)
  | fullyExpandBindingTrivpat varenv (pat as RECORDpat{fields,...})=
      mkRECORDpat pat (map (fullyExpandBindingTrivpat varenv o #2) fields)
  | fullyExpandBindingTrivpat varenv (CONSTRAINTpat(pat, _)) =
      fullyExpandBindingTrivpat varenv pat
  | fullyExpandBindingTrivpat _ _ = 
      impossible "fullyExpandBindingTrivpat miscalled"
 
end (* toplevel local *)
end (* structure TemplateExpansion *)


