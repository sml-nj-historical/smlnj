(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* matchcomp.sml *)

signature MATCH_COMP =
sig

  type toTcLt = (Types.ty -> PLambdaType.tyc) * (Types.ty -> PLambdaType.lty)

  type genintinfswitch =
       PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
       -> PLambda.lexp

  val bindCompile : 
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list 
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer
	  * genintinfswitch
	-> PLambda.lexp

  val matchCompile : 
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list 
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer
	  * genintinfswitch
	-> PLambda.lexp

  val handCompile : 
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list 
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer * genintinfswitch
	-> PLambda.lexp

end (* signature MATCH_COMP *)


structure MatchComp : MATCH_COMP =
struct

local structure DA = Access
      structure BT = BasicTypes
      structure LT = PLambdaType
      structure TU = TypesUtil
      structure PO = PrimOp
      structure MP = PPLexp
      structure EM = ErrorMsg
      structure TP = Types
      structure LN = LiteralToNum
      structure PP = PrettyPrint

      open VarCon Types
      open Absyn PLambda         
      open PrettyPrint
      open TemplateExpansion MCCommon

in 

val intersect=SortedList.intersect
val union = SortedList.merge
val setDifference = SortedList.difference
fun isthere(i,set) = SortedList.member set i

fun bug s = EM.impossible ("MatchComp: " ^ s)
val say = Control.Print.say
type toTcLt = (ty -> LT.tyc) * (ty -> LT.lty)

type genintinfswitch =
     PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
     -> PLambda.lexp

(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken 
 * from the LambdaVar module; I think it should be taken from the 
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 *)
val mkv = LambdaVar.mkLvar

fun abstest0 _ = bug "abstest0 unimplemented"
fun abstest1 _ = bug "abstest1 unimplemented"

(** translating the typ field in DATACON into lty; constant datacons 
    will take ltc_unit as the argument *)
fun toDconLty toLty ty =
  (case ty 
    of TP.POLYty{sign, tyfun=TYFUN{arity, body}} =>
         if BT.isArrowType body then toLty ty
         else toLty (TP.POLYty{sign=sign, 
                               tyfun=TYFUN{arity=arity,
                                           body=BT.-->(BT.unitTy, body)}})
     | _ => if BT.isArrowType ty then toLty ty
            else toLty (BT.-->(BT.unitTy, ty)))

(**************************************************************************)

datatype andor
  = AND of {bindings : (int * var) list,
            subtrees : andor list,
            constraints : (dconinfo * int list * andor option) list}
  | CASE of {bindings : (int * var) list,
             sign : DA.consig,
             cases : (pcon * int list * andor list) list,
             constraints : (dconinfo * int list * andor option) list}
  | LEAF of {bindings : (int * var) list,
             constraints : (dconinfo * int list * andor option) list}

datatype decision
  = CASEDEC of path * DA.consig * 
               (pcon * int list * decision list) list * int list
  | ABSCONDEC of path * dconinfo * int list * decision list * int list 
  | BINDDEC of path * int list

fun allConses(hds, tls) = 
  List.concat (map (fn hd => (map (fn tl => hd::tl) tls)) hds)

fun orExpand (ORpat(pat1,pat2)) = 
      (orExpand pat1)@(orExpand pat2) 
  | orExpand (pat as RECORDpat{fields,...}) =
     map (mkRECORDpat pat) (foldr allConses [nil] (map (orExpand o #2) fields))
  | orExpand (VECTORpat(pats,t)) =
      map (fn p => VECTORpat(p,t)) (foldr allConses [nil] (map orExpand pats))
  | orExpand (APPpat(k,t,pat)) =
      map (fn pat => APPpat(k,t,pat)) (orExpand pat)
  | orExpand (CONSTRAINTpat(pat,_)) =
      orExpand pat
  | orExpand (LAYEREDpat(CONSTRAINTpat(lpat, _), bpat)) =
      orExpand (LAYEREDpat(lpat, bpat))
  | orExpand (LAYEREDpat(lpat, bpat)) =
      map (fn pat => LAYEREDpat(lpat,pat)) (orExpand bpat)
  | orExpand pat = 
      [pat]

fun lookupVar (v as VALvar{path=p1,...},
               (VALvar{path=p2,...}, value)::rest) =
       if SymPath.equal(p1,p2) then value else lookupVar(v, rest)
  | lookupVar (VALvar _, []) = bug "unbound 18"
  | lookupVar _ = bug "[MC.lookupVar]"

fun pathInstSimpexp varenv (VARsimp v) = lookupVar (v, varenv)
  | pathInstSimpexp varenv (RECORDsimp labsimps) = 
      RECORDPATH (map (pathInstSimpexp varenv o #2) labsimps)

fun expandBindings (varenv, pathenv, nil) = nil
  | expandBindings (varenv, pathenv, v::rest) =
      (pathInstSimpexp pathenv (fullyExpandBinding varenv (VARsimp v)))
        :: (expandBindings(varenv, pathenv, rest))

fun boundVariables (VARpat v) = [v]
  | boundVariables (CONSTRAINTpat(pat,_)) = boundVariables pat
  | boundVariables (LAYEREDpat(pat1, pat2)) = 
      (boundVariables(pat1))@(boundVariables(pat2))
  | boundVariables (APPpat(k,t,pat)) = boundVariables pat
  | boundVariables (RECORDpat{fields,...}) =
      List.concat (map (boundVariables o #2) fields)
  | boundVariables (VECTORpat(pats,_)) = List.concat (map boundVariables pats)
  | boundVariables (ORpat (pat1,_)) = boundVariables pat1
  | boundVariables _ = nil

fun patternBindings (VARpat v, path) = [(v, path)]
  | patternBindings (CONSTRAINTpat(pat,_), path) = patternBindings(pat, path)
  | patternBindings (LAYEREDpat(pat1, pat2), path) = 
      (patternBindings(pat1, path))@(patternBindings(pat2, path))
  | patternBindings (APPpat(k,t,pat), path) = 
      patternBindings(pat, DELTAPATH(DATApcon(k, t), path))
  | patternBindings (RECORDpat{fields,...}, path) = 
      let fun doGen(n, nil) = nil
            | doGen(n, (lab,pat)::rest) = 
                (patternBindings(pat,PIPATH(n,path))) @ (doGen(n+1,rest))
       in doGen(0, fields)
      end
  | patternBindings (VECTORpat(pats,t), path) = 
      let fun doGen(n, nil) = nil
            | doGen(n, pat::rest) = 
                (patternBindings(pat,VPIPATH(n,t,path))) @ (doGen(n+1,rest))
       in doGen(0, pats)
      end
  | patternBindings (ORpat _, _) = bug "Unexpected or pattern"
  | patternBindings _ = nil
 
fun patPaths (pat, constrs) =
  let val patEnv = patternBindings(pat, ROOTPATH)
      fun constrPaths (nil, env, acc) = 
            ((ROOTPATH, pat)::(rev acc), env)
        | constrPaths ((simpexp,cpat)::rest, env, acc) = 
            let val guardPath = pathInstSimpexp env simpexp
                val newEnv = patternBindings(cpat, guardPath)
             in constrPaths(rest, env@newEnv, (guardPath, cpat)::acc)
            end
   in constrPaths(constrs, patEnv, nil)
  end

fun vartolvar (VALvar{access=DA.LVAR v, typ,...}, toLty) = (v, toLty (!typ))
  | vartolvar _ = bug "bug variable in mc.sml"

fun preProcessPat toLty (pat, rhs) =
  let val bindings = boundVariables pat
      val fname = mkv()

      fun genRHSFun ([], rhs) = FN(mkv(), LT.ltc_unit, rhs)
        | genRHSFun ([v], rhs) = 
            let val (argvar,argt) = vartolvar(v, toLty)
             in FN(argvar,argt,rhs)
            end
        | genRHSFun (vl, rhs) =
            let val argvar = mkv()
                fun foo (nil, n) = (rhs,nil)
                  | foo (v::vl, n) = 
                      let val (lv,lt) = vartolvar(v, toLty)
                          val (le,tt) = foo(vl,n+1)
                       in (LET(lv, SELECT(n,VAR argvar), le), lt :: tt)
                      end
                val (body,tt) = foo(vl,0)
             in FN(argvar, LT.ltc_tuple tt, body)
            end

      val rhsFun = genRHSFun (bindings, rhs)
      val pats = orExpand pat
      fun expand nil = nil
        | expand (pat::rest) =
            let val (newpat, constrs, varenv) = templateExpandPattern pat
                val (newlist, pathenv) = patPaths (newpat, constrs)
                val bindingPaths = expandBindings(varenv,pathenv,bindings)
             in (newlist, bindingPaths, fname)::(expand rest)
            end handle CANT_MATCH =>
                  ([(ROOTPATH, NOpat)], nil, fname)::(expand rest)
   in (expand pats, (fname, rhsFun))
  end

fun makeAndor (matchRep,err) =
let fun addBinding (v, rule, AND{bindings, subtrees, constraints}) =
	  AND{bindings=(rule,v)::bindings, subtrees=subtrees, 
	      constraints=constraints}
      | addBinding (v, rule, CASE{bindings, sign, cases, constraints}) =
	  CASE{bindings=(rule,v)::bindings, cases=cases, sign = sign,
	       constraints=constraints}
      | addBinding (v, rule, LEAF{bindings, constraints}) =
	  LEAF{bindings=(rule,v)::bindings, constraints=constraints}

    fun wordCon(s, t, msg) = 
	let fun conv(wrapFn,convFn) =
	        wrapFn(convFn s handle Overflow =>
		       (err EM.COMPLAIN
			   ("out-of-range word literal in pattern: 0w"
			    ^IntInf.toString s)
			   EM.nullErrorBody;
                       convFn(IntInf.fromInt 0)))
	 in if TU.equalType(t,BT.wordTy) then
	      conv(WORDpcon,LN.word)  (* WORDpcon(LN.word s) *)
	    else if TU.equalType(t,BT.word8Ty) then
	      conv(WORDpcon,LN.word8) (* WORDpcon(LN.word8 s) *)
	    else if TU.equalType(t,BT.word32Ty) then
	      conv(WORD32pcon,LN.word32) (* WORD32pcon(LN.word32 s) *)
	    else bug msg
	end

    fun numCon(s, t, msg) = 
      if TU.equalType(t,BT.intTy) then
	    INTpcon(LN.int s) 
      else if TU.equalType(t,BT.int32Ty) then
	    INT32pcon (LN.int32 s)
      else if TU.equalType (t, BT.intinfTy) then
	  INTINFpcon s
      else wordCon(s, t, msg)

    fun addAConstraint(k, NONE, rule, nil) = [(k, [rule], NONE)]
      | addAConstraint(k, SOME pat, rule, nil) =
	  [(k, [rule], SOME(genAndor(pat, rule)))]
      | addAConstraint(k, patopt as SOME pat, rule, 
		       (constr as (k', rules, SOME subtree))::rest) =
	  if conEq'(k, k') then
	    (k, rule::rules, SOME(mergeAndor(pat, subtree, rule)))::rest
	  else 
	    constr::(addAConstraint(k, patopt, rule, rest))
      | addAConstraint(k, NONE, rule, (constr as (k', rules, NONE))::rest) =
	  if conEq'(k, k') then (k, rule::rules, NONE)::rest
	  else constr::(addAConstraint(k, NONE, rule, rest))
      | addAConstraint(k, patopt, rule, (constr as (k', rules, _))::rest) =
	      if conEq'(k, k') then bug "arity conflict"
	  else constr::(addAConstraint(k, patopt, rule, rest))

    and addConstraint(k, patopt, rule, AND{bindings, subtrees, constraints}) =
	  AND{bindings=bindings, subtrees=subtrees, 
	      constraints=addAConstraint(k, patopt, rule, constraints)}
      | addConstraint(k, patopt, rule, CASE{bindings, sign, cases, 
					    constraints}) =
	  CASE{bindings=bindings, cases=cases, sign = sign,
	       constraints=addAConstraint(k, patopt, rule, constraints)}
      | addConstraint(k, patopt, rule, LEAF{bindings, constraints}) =
	  LEAF{bindings=bindings, 
	       constraints=addAConstraint(k, patopt, rule, constraints)}

    and genAndor (VARpat v, rule) =
	  LEAF {bindings = [(rule, v)], constraints = nil}
      | genAndor (WILDpat, rule) =
	  LEAF {bindings = nil, constraints = nil}
      | genAndor (CONSTRAINTpat(pat, _), rule) =  genAndor(pat, rule)
      | genAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), rule) =
	  genAndor (LAYEREDpat(lpat, bpat), rule)
      | genAndor (LAYEREDpat(VARpat v, bpat), rule) =
	  addBinding (v, rule, genAndor (bpat, rule))
      | genAndor (LAYEREDpat(CONpat(k,t), bpat), rule) =
	  addConstraint ((k,t), NONE, rule, genAndor(bpat, rule))
      | genAndor (LAYEREDpat(APPpat(k,t,lpat), bpat), rule) =
	  addConstraint ((k,t), SOME lpat, rule, genAndor(bpat, rule))
      | genAndor (INTpat (s,t), rule) = 
	  if TU.equalType (t, BT.int64Ty) then genAndor64 (LN.int64 s, rule)
	  else let val con = numCon(s, t, "genAndor INTpat")
	       in CASE{bindings = nil, constraints = nil, sign = DA.CNIL,
		       cases = [(con, [rule], nil)]}
	       end
      | genAndor (WORDpat(s,t), rule) = 
	  if TU.equalType (t, BT.word64Ty) then genAndor64 (LN.word64 s, rule)
	  else let val con = wordCon(s, t, "genAndor WORDpat")
	       in CASE{bindings = nil, constraints = nil, sign = DA.CNIL,
		       cases = [(con, [rule], nil)]}
	       end
      | genAndor (REALpat r, rule) =
	  CASE {bindings = nil, constraints = nil, sign = DA.CNIL,
		cases = [(REALpcon r, [rule], nil)]}
      | genAndor (STRINGpat s, rule) =
	  CASE {bindings = nil, constraints = nil, sign = DA.CNIL,
		cases = [(STRINGpcon s, [rule], nil)]}

	(*
	 * NOTE: the following won't work for cross compiling 
	 *       to multi-byte characters. 
	 *)
      | genAndor (CHARpat s, rule) =
	  CASE {bindings = nil, constraints = nil, sign = DA.CNIL,
		cases = [(INTpcon (Char.ord(String.sub(s, 0))), [rule], nil)]}
      | genAndor (RECORDpat{fields,...}, rule) =
	  AND{bindings = nil, constraints = nil, 
	      subtrees=multiGen(map #2 fields, rule)}
      | genAndor (VECTORpat(pats,t), rule) =
	  CASE {bindings = nil, constraints = nil, sign = DA.CNIL,
		cases = [(VLENpcon (length pats, t), [rule], 
			  multiGen(pats, rule))]}
      | genAndor (CONpat(k,t), rule) =
	  if abstract k then
	    LEAF {bindings = nil, constraints = [((k, t), [rule], NONE)]}
	  else
	    CASE {bindings = nil, constraints = nil, sign = signOfCon k,
		  cases = [(DATApcon(k, t), [rule], nil)]}
      | genAndor (APPpat(k,t,pat), rule) =
	  if abstract k then
	    LEAF {bindings = nil, 
		  constraints = [((k,t), [rule], SOME(genAndor (pat, rule)))]}
	  else
	    CASE {bindings = nil, constraints = nil, sign = signOfCon k,
		  cases = [(DATApcon(k,t), [rule], [genAndor(pat, rule)])]}
      | genAndor _ =
	  bug "genandor applied to inapplicable pattern"

    (* simulate 64-bit words and ints as pairs of 32-bit words *)
    and genAndor64 ((hi, lo), rule) =
	let fun p32 w = WORDpat (Word32.toLargeInt w, BT.word32Ty)
	in genAndor (AbsynUtil.TUPLEpat [p32 hi, p32 lo], rule)
	end

    and multiGen(nil, rule) = nil
      | multiGen(pat::rest, rule) = (genAndor(pat,rule))::multiGen((rest,rule))

    and mergeAndor (VARpat v, andor, rule) = addBinding (v, rule, andor)
      | mergeAndor (WILDpat, andor, rule) = andor
      | mergeAndor (CONSTRAINTpat(pat, _), andor, rule) = 
	  mergeAndor(pat, andor, rule)
      | mergeAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), andor, rule) =
	  mergeAndor (LAYEREDpat(lpat, bpat), andor, rule)
      | mergeAndor (LAYEREDpat(VARpat v, bpat), andor, rule) =
	  addBinding (v, rule, mergeAndor (bpat, andor, rule))
      | mergeAndor (LAYEREDpat(CONpat(k,t), bpat), andor, rule) =
	  addConstraint ((k,t), NONE, rule, mergeAndor(bpat, andor, rule))
      | mergeAndor (LAYEREDpat(APPpat(k,t,lpat), bpat), andor, rule) =
	  addConstraint ((k,t), SOME lpat, rule, mergeAndor(bpat, andor, rule))
      | mergeAndor (CONpat(k,t), LEAF{bindings, constraints}, rule) =
	  if abstract k then
	    LEAF {bindings = nil, 
		  constraints = addAConstraint((k, t), NONE, rule, constraints)}
	  else
	    CASE {bindings = nil, constraints = nil, sign = signOfCon k,
		  cases = [(DATApcon(k,t), [rule], nil)]}
      | mergeAndor (APPpat(k,t,pat), LEAF{bindings, constraints}, rule) =
	  if abstract k then
	    LEAF {bindings = bindings,
		  constraints = addAConstraint((k,t), SOME pat, rule, constraints)}
	  else
	    CASE {bindings = bindings, constraints = constraints, 
		  sign = signOfCon k,
		  cases = [(DATApcon(k,t), [rule], [genAndor(pat, rule)])]}
      | mergeAndor (pat, LEAF{bindings, constraints}, rule) =
	  (case genAndor(pat, rule)
	     of CASE{bindings=nil, constraints=nil, sign, cases} =>
		  CASE{bindings=bindings, sign=sign, 
		       constraints=constraints, cases=cases}
	      | AND{bindings=nil, constraints=nil, subtrees} =>
		  AND{bindings=bindings, constraints=constraints, 
		      subtrees=subtrees}
	      | _ => bug "genAndor returned bogusly")
      | mergeAndor (INTpat (s,t), c as CASE{bindings, cases, 
					    constraints, sign}, rule) =
	  if TU.equalType (t, BT.int64Ty) then
	      mergeAndor64 (LN.int64 s, c, rule)
	  else let val pcon = numCon(s, t, "mergeAndor INTpat")
	       in CASE{bindings = bindings, constraints = constraints,
		       sign = sign, cases = addACase(pcon, nil, rule, cases)}
	       end
      | mergeAndor (WORDpat(s,t), c as CASE{bindings, cases, 
					    constraints, sign}, rule) =
	  if TU.equalType (t, BT.word64Ty) then
	      mergeAndor64 (LN.word64 s, c, rule)
	  else let val pcon = wordCon(s, t, "mergeAndor WORDpat")
	       in CASE{bindings = bindings, constraints = constraints,
		       sign = sign, cases = addACase(pcon, nil, rule, cases)}
	       end
      | mergeAndor (REALpat r, CASE{bindings, cases, constraints,sign}, rule) =
	  CASE {bindings = bindings, constraints = constraints, sign=sign,
		cases = addACase(REALpcon r, nil, rule, cases)}
      | mergeAndor (STRINGpat s, CASE{bindings, cases, constraints,sign}, rule) =
	  CASE {bindings = bindings, constraints = constraints, sign=sign,
		cases = addACase(STRINGpcon s, nil, rule, cases)}

      (*
       * NOTE: the following won't work for cross compiling 
       * to multi-byte characters 
       *)
      | mergeAndor (CHARpat s, CASE{bindings, cases, 
				    constraints, sign}, rule) =
	  CASE {bindings = bindings, constraints = constraints, sign=sign,
		cases = addACase(INTpcon(Char.ord(String.sub(s, 0))), 
				 nil, rule, cases)}

      | mergeAndor (RECORDpat{fields,...}, 
		    AND{bindings, constraints, subtrees}, rule) =
	  AND{bindings = bindings, constraints = constraints, 
	      subtrees=multiMerge(map #2 fields, subtrees, rule)}
      | mergeAndor (VECTORpat(pats,t), CASE{bindings, cases, sign, 
					    constraints}, rule) =
	  CASE {bindings = bindings, constraints = constraints, sign = sign,
		cases = addACase(VLENpcon(length pats, t),pats,rule,cases)}
      | mergeAndor (CONpat(k,t), CASE{bindings, 
				      cases, constraints, sign}, rule) =
	  if abstract k then
	    CASE {bindings=bindings, cases=cases, sign=sign,
		  constraints=addAConstraint((k,t), NONE, rule, constraints)}
	  else
	    CASE {bindings=bindings, constraints=constraints, sign=sign,
		  cases=addACase(DATApcon(k,t), nil, rule, cases)}
      | mergeAndor (APPpat(k,t,pat), CASE{bindings, cases, 
					  constraints, sign}, rule) =
	  if abstract k then
	    CASE {bindings=bindings, cases=cases,  sign=sign,
		  constraints=addAConstraint((k,t), SOME pat, rule, constraints)}
	  else
	    CASE {bindings=bindings, constraints=constraints, sign=sign,
		  cases=addACase(DATApcon(k,t), [pat], rule, cases)}
      | mergeAndor (CONpat(k,t), AND{bindings, constraints, subtrees}, rule) =
	  if abstract k then
	    AND {bindings=bindings, subtrees=subtrees,
		 constraints=addAConstraint((k,t), NONE, rule, constraints)}
	  else bug "concrete constructor can't match record"
      | mergeAndor (APPpat(k,t,pat), AND{bindings,subtrees,constraints}, rule) =
	  if abstract k then
	    AND {bindings=bindings, subtrees=subtrees,
		 constraints=addAConstraint((k,t), SOME pat, rule, constraints)}
	  else bug "concrete constructor application can't match record"
      | mergeAndor _ =
	  bug "bad pattern merge"

    (* simulate 64-bit words and ints as pairs of 32-bit words *)
    and mergeAndor64 ((hi, lo), c, rule) =
	let fun p32 w = WORDpat (Word32.toLargeInt w, BT.word32Ty)
	in mergeAndor (AbsynUtil.TUPLEpat [p32 hi, p32 lo], c, rule)
	end

    and addACase (pcon, pats, rule, nil) =
	  [(pcon, [rule], multiGen(pats, rule))]
      | addACase (pcon, pats, rule, 
		 (aCase as (pcon', rules,subtrees))::rest) =
	  if constantEq(pcon, pcon') then
	    (pcon, rule::rules, multiMerge(pats, subtrees, rule))::rest
	  else 
	    aCase::(addACase(pcon, pats, rule, rest))

    and multiMerge (nil, nil, rule) = nil
      | multiMerge (pat::pats, subtree::subtrees, rule) =
	 (mergeAndor(pat, subtree, rule))::(multiMerge(pats, subtrees, rule))
      | multiMerge _ = bug "list length mismatch in multiMerge"


    fun mergePatWithAndorList(path, pat, nil, n) =
	  [(path, genAndor(pat, n))]
      | mergePatWithAndorList(path, pat, (path',andor)::rest, n) =
	  if pathEq(path, path') then (path, mergeAndor(pat, andor, n))::rest
	  else (path',andor)::(mergePatWithAndorList(path, pat, rest, n))

    fun genAndorList (nil, n) = bug "no patterns (gen)"
      | genAndorList ([(path, pat)], n) = [(path, genAndor(pat, n))]
      | genAndorList ((path, pat)::rest, n) = 
	      mergePatWithAndorList(path, pat, genAndorList(rest, n), n)

    fun mergeAndorList (nil, aol, n) = bug "no patterns (merge)"
      | mergeAndorList ([(path, pat)], aol, n) = 
	  mergePatWithAndorList(path, pat, aol, n)
      | mergeAndorList ((path, pat)::rest, aol, n) = 
	      mergePatWithAndorList(path, pat, mergeAndorList(rest, aol, n), n)

    fun makeAndor' (nil, n) = bug "no rules (makeAndor')"
      | makeAndor' ([(pats, _, _)], n) = 
	  genAndorList (pats, n)
      | makeAndor' (([(_, NOpat)], env, bindings)::rest, n) =
	  makeAndor'(rest, n+1)
      | makeAndor' ((pats, env, bindings)::rest, n) =
	  mergeAndorList(pats, makeAndor'(rest, n+1), n)

in makeAndor' (matchRep,0) (* handle Foo => raise (Internal 99) *)
end (* fun makeAndor *)

fun addABinding (path, rule, nil) = [BINDDEC(path, [rule])]
  | addABinding (path, rule, (bind as BINDDEC(path', rules))::rest) =
      if pathEq(path, path') then BINDDEC(path, rule::rules)::rest
      else bind::(addABinding(path, rule, rest)) 
  | addABinding _ = bug "non BINDDEC in binding list"

fun flattenBindings (nil, path, active) = nil
  | flattenBindings (((rule, v)::rest), path, active) =
      if isthere(rule, active) then 
	addABinding(path, rule, flattenBindings(rest, path,active))
      else 
	flattenBindings(rest, path, active)

fun flattenConstraints (nil, path, active) = nil
  | flattenConstraints ((di,rules,NONE)::rest, path, active) = 
      let val yesActive = intersect(active, rules)
	  val noActive = setDifference(active, rules)
	  val rest' = flattenConstraints(rest, path, active)
       in (ABSCONDEC(path, di, yesActive, nil, noActive))::rest'
      end
  | flattenConstraints ((di,rules,SOME andor)::rest, path, active) = 
      let val yesActive = intersect(active, rules)
	  val noActive = setDifference(active, rules)
	  val rest' = flattenConstraints(rest, path, active)
	  val andor' = 
	    flattenAndor(andor, DELTAPATH(DATApcon di, path), active)
       in (ABSCONDEC(path, di, yesActive, andor', noActive))::rest'
      end     

and flattenAndor (AND {bindings, subtrees, constraints}, path, active) =
      let val btests = flattenBindings(bindings, path, active)
	  fun dotree (n, nil) =
		flattenConstraints(constraints, path, active)
	    | dotree (n, subtree::rest) =
		let val othertests = dotree(n + 1, rest)
		 in (flattenAndor(subtree,PIPATH(n,path),active))@othertests
		end
       in btests@(dotree(0, subtrees))
      end
  | flattenAndor (CASE {bindings, cases, constraints,sign}, path, active) =
      let val btests = flattenBindings(bindings, path, active)
	  val ctests = flattenConstraints(constraints, path, active)
       in btests@((flattenCases(cases, path, active,sign))::ctests)
      end
  | flattenAndor (LEAF {bindings, constraints}, path, active) =       
      let val btests = flattenBindings(bindings, path, active)
       in btests@(flattenConstraints(constraints, path, active))
      end

and flattenACase((VLENpcon(n, t), rules, subtrees),path,active,defaults) =
      let val stillActive = intersect(union(rules, defaults), active)
	  val ruleActive = intersect(rules, active)
	  fun flattenVSubs (n, nil) = nil
	    | flattenVSubs (n, subtree::rest) = 
		 (flattenAndor(subtree, VPIPATH(n,t,path), stillActive)) 
		 @ (flattenVSubs(n + 1, rest))
       in (INTpcon n, ruleActive, flattenVSubs(0, subtrees))
      end     
  | flattenACase((k as DATApcon (_,t), rules,[subtree]),path,active,defaults) =
      let val stillActive = intersect(union(rules, defaults), active)
	  val ruleActive = intersect(rules, active)
	  val newPath = DELTAPATH(k,path)                    
       in (k,ruleActive,flattenAndor(subtree,newPath,stillActive))
      end 
  | flattenACase((constant,rules,nil),path,active,defaults) =
      (constant, intersect(rules, active), nil)
  | flattenACase _ =
      bug "illegal subpattern in a case"

and flattenCases(cases, path, active,sign) =
  let fun calcDefaults (nil, active) = active
	| calcDefaults ((_,rules,_)::rest, active)  =
	    calcDefaults(rest, setDifference(active, rules))
      val defaults = calcDefaults(cases, active)
      fun doit nil = nil
	| doit (aCase::rest) = 
	    ((flattenACase(aCase, path, active, defaults)) 
	     :: (doit(rest)))
   in case cases
       of (VLENpcon (_,t), _, _)::_ => 
	     CASEDEC(VLENPATH(path, t), sign, doit cases, defaults)
	| cases => CASEDEC(path, sign, doit cases, defaults)
  end

fun bindings (n, l) = case (List.nth(l, n)) of (_,_,x) => x

fun pathConstraints (RECORDPATH paths) = 
          List.concat (map pathConstraints paths)
  | pathConstraints path = [path]

fun flattenAndors(nil, allrules) = nil
  | flattenAndors((path, andor)::rest, allrules) =
      (pathConstraints path, flattenAndor(andor, path, allrules))
        :: (flattenAndors(rest, allrules))

fun removePath(path, path1::rest) =
      if pathEq(path, path1) then rest 
      else path1::(removePath(path, rest))
  | removePath (path, nil) = nil

fun fireConstraint (path, (needPaths, decisions)::rest, ready, delayed) =
      (case removePath(path, needPaths) 
         of nil => fireConstraint(path, rest, decisions@ready, delayed)
          | x => fireConstraint(path, rest, ready, (x,decisions)::delayed))
  | fireConstraint (path, nil, ready, delayed) =
      (ready, delayed)

fun mkAllRules (nil,_) = nil 
  | mkAllRules(([(ROOTPATH, NOpat)],_,_)::b, n) = (mkAllRules(b, n + 1))
  | mkAllRules(_::b, n) = n::(mkAllRules(b, n + 1))

exception PickBest

fun relevent (CASEDEC(_,_,_,defaults), rulenum) = 
      not (isthere(rulenum, defaults))
  | relevent (ABSCONDEC (_,_,_,_,defaults), rulenum) =
      not (isthere(rulenum, defaults))
  | relevent (BINDDEC _, _) = 
      bug "BINDDEC not fired"

fun metric (CASEDEC(_,_,cases, defaults)) = (length defaults, length cases)
  | metric (ABSCONDEC (_,_,_,_,defaults)) = (length defaults, 2)
  | metric (BINDDEC _) = bug "BINDDEC not fired (metric)"

fun metricBetter((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d)

fun doPickBest(nil, _, _, _, NONE) = raise PickBest
  | doPickBest(nil, _, _, _, SOME n) = n
  | doPickBest((BINDDEC _)::rest, _, n, _, _) = n
  | doPickBest((CASEDEC(_, DA.CSIG(1,0), _, _))::rest, _, n, _, _) = n
  | doPickBest((CASEDEC(_, DA.CSIG(0,1), _, _))::rest, _, n, _, _) = n
  | doPickBest(aCase::rest, active as act1::_, n, NONE, NONE) =
	  if relevent (aCase, act1) then
	    doPickBest(rest, active, n + 1, SOME(metric aCase), SOME n)
	  else 
	    doPickBest(rest, active, n + 1, NONE, NONE)
  | doPickBest(aCase::rest, active as act1::_, n, SOME m, SOME i) =
	  if relevent (aCase, act1) then
	    let val myMetric = metric aCase
	    in
	      if metricBetter(myMetric, m) then
            doPickBest(rest, active, n + 1, SOME(myMetric), SOME n)
	      else 
            doPickBest(rest, active, n + 1, SOME m, SOME i)
        end
	  else 
	    doPickBest(rest, active, n + 1, SOME m, SOME i)
  | doPickBest _ = bug "bug situation in doPickBest"

fun pickBest (l, active) = doPickBest(l, active, 0, NONE, NONE)

fun extractNth(0, a::b) = (a, b)
  | extractNth(n, a::b) = 
      let val (c,d) = extractNth(n - 1, b) in (c, a::d) end
  | extractNth _ = bug "extractNth called with too big n"

fun filter (f, nil) = nil | 
    filter (f, a::b) = if f a then a::(filter(f,b)) else filter(f,b)

fun genDecisionTree((decisions, delayed), active as active1::_) =
      ((case extractNth(pickBest(decisions, active), decisions)
         of (BINDDEC(path, _), rest) =>
	          genDecisionTree(fireConstraint(path,delayed,rest,nil),active)
(*
          | (CASEDEC(path, DA.CSIG(1,0), 
                     [(_,_,guarded)], defaults), rest) => 
              genDecisionTree((rest@guarded, delayed), active)
          | (CASEDEC(path, DA.CSIG(0,1), 
                     [(_,_,guarded)], defaults), rest) => 
              genDecisionTree((rest@guarded, delayed), active)
*)
          | (CASEDEC(path, sign, cases, defaults), rest) =>
             let fun isActive(_,rules,_) = intersect(rules, active) <> []
                 val activeCases = filter(isActive, cases)
                 val caseTrees = 
                   gencases(activeCases, rest, delayed, defaults, active)
                 val defActive = intersect(active, defaults)
                 fun len (DA.CSIG(i,j)) = i+j
                   | len (DA.CNIL) = 0
		 val defTree = 
                   if length activeCases = len sign then NONE 
                   else SOME (genDecisionTree((rest, delayed), defActive))
              in CASETEST(path, sign, caseTrees, defTree)
             end
          | (ABSCONDEC(path, con, yes, guarded, defaults), rest) =>
	     let val yesActive = intersect(active, union(yes, defaults))
                 val noActive = intersect(active,defaults)
                 val yesTree = 
                      genDecisionTree((rest@guarded, delayed), yesActive)
                 val defTree = genDecisionTree((rest, delayed), noActive)
              in if unary con then ABSTEST1(path, con, yesTree, defTree)
	         else ABSTEST0(path, con, yesTree, defTree)
             end)
       handle PickBest => (RHS active1))
  | genDecisionTree (_,active) = bug "nothing active"

and gencases (nil, decs, delayed, defaults, active) = nil
  | gencases ((pcon,rules,guarded)::rest,decs,delayed,defaults,active)= 
      let val rActive = intersect(union(defaults, rules), active)
       in (pcon, genDecisionTree((decs@guarded, delayed),rActive))
          :: (gencases(rest,decs,delayed,defaults,active))
      end

local open PrintUtil
      val printDepth = Control.Print.printDepth
in

fun matchPrint (env,rules,unused) ppstrm =        
  let fun matchPrint' ([],_,_) = ()
        | matchPrint' ([(pat,_)],_,_) = () (* never print last rule *)
        | matchPrint' ((pat,_)::more,[],_) =
           (PP.string ppstrm "        "; 
            PPAbsyn.ppPat env ppstrm (pat,!printDepth);
            PP.string ppstrm " => ...";
            PP.newline ppstrm;
            matchPrint' (more,[],0))
        | matchPrint' ((pat,_)::more,(taglist as (tag::tags)),i) =
           if i = tag then
            (PP.string ppstrm "  -->   ";
             PPAbsyn.ppPat env ppstrm (pat,!printDepth);
             PP.string ppstrm " => ..."; 
             PP.newline ppstrm;
             matchPrint'(more,tags,i+1))
           else 
            (PP.string ppstrm "        ";
             PPAbsyn.ppPat env ppstrm (pat,!printDepth);
             PP.string ppstrm " => ...";
             PP.newline ppstrm;
             matchPrint'(more,taglist,i+1))
   in PP.newline ppstrm;
      PP.openHVBox ppstrm (PP.Rel 0);
      matchPrint'(rules,unused,0);
      PP.closeBox ppstrm
  end

fun bindPrint (env,(pat,_)::_) ppstrm =
      (PP.newline ppstrm; PP.string ppstrm "        "; 
       PPAbsyn.ppPat env ppstrm (pat,!printDepth);
       PP.string ppstrm " = ...")
  | bindPrint _ _ = bug "bindPrint in mc"

end (* local printutil *)

fun rulesUsed (RHS n) = [n]
  | rulesUsed (BIND(_, dt)) = rulesUsed dt
  | rulesUsed (CASETEST(_, _, cases, NONE)) =
      foldr (fn((_,a), b) => union(rulesUsed a, b)) nil cases
  | rulesUsed (CASETEST(_, _, cases, SOME dt)) =
      foldr (fn((_,a), b) => union(rulesUsed a, b)) (rulesUsed dt) cases
  | rulesUsed (ABSTEST0(_, _, yes, no)) = 
      union(rulesUsed yes, rulesUsed no)
  | rulesUsed (ABSTEST1(_, _, yes, no)) = 
      union(rulesUsed yes, rulesUsed no)

fun fixupUnused(nil, _, _, _, out) = out
  | fixupUnused(unused, (nil, _)::rest, n, m, out) = 
      fixupUnused(unused, rest, n, m + 1, out)
  | fixupUnused(unused::urest, (rule::rules, x)::mrest, n, m, nil) =
      if unused = n then 
       fixupUnused(urest, (rules, x)::mrest, n + 1, m, [m])
      else 
       fixupUnused(unused::urest, (rules, x)::mrest, n + 1, m, nil)
  | fixupUnused(unused::urest, (rule::rules, z)::mrest, n, m, x::y) =
      if unused = n then
       (if m <> x then
          fixupUnused(urest, (rules, z)::mrest, n + 1, m, m::x::y)
        else fixupUnused(urest, (rules, z)::mrest, n + 1, m, x::y))
      else fixupUnused(unused::urest, (rules, z)::mrest, n + 1, m, x::y)
  | fixupUnused _ = bug "bad fixup"

fun redundant (nil, n: int) = false
  | redundant (a::b, n) = a <> n orelse redundant (b, n)

fun complement(n, m, a::b) =
      if n < a then n::(complement(n + 1, m, a::b))
      else complement(n + 1, m, b)
  | complement(n, m, nil) = 
      if n < m then n::(complement(n + 1, m, nil)) else nil

fun dividePathList(pred, nil, accyes, accno) = (accyes, accno)
  | dividePathList(pred, path::rest, accyes, accno) = 
      if pred path then dividePathList(pred, rest, path::accyes, accno)
      else dividePathList(pred, rest, accyes, path::accno)

fun addPathToPathList (path, path1::rest) = 
      if pathEq(path, path1) then path1::rest
      else path1::(addPathToPathList(path, rest))
  | addPathToPathList (path, nil) = [path]

fun unitePathLists(paths1, nil) = paths1
  | unitePathLists(nil, paths2) = paths2
  | unitePathLists(path1::rest1, paths2) = 
      addPathToPathList(path1, unitePathLists(rest1, paths2))

fun onPathList (path1, nil) = false
  | onPathList (path1, path2::rest) = 
      pathEq(path1, path2) orelse onPathList(path1, rest)

fun intersectPathLists(paths1, nil) = nil
  | intersectPathLists(nil, paths2) = nil
  | intersectPathLists(path1::rest1, paths2) = 
      if onPathList(path1,paths2) then 
        path1::(intersectPathLists(rest1, paths2))
      else
        intersectPathLists(rest1, paths2)

fun differencePathLists(paths1, nil) = paths1
  | differencePathLists(nil, paths2) = nil
  | differencePathLists(path1::rest1, paths2) = 
      if onPathList(path1,paths2) then 
        differencePathLists(rest1, paths2)
      else
        path1::(differencePathLists(rest1, paths2))

fun intersectPathsets(pathset1, nil) = nil
  | intersectPathsets(nil, pathset2) = nil
  | intersectPathsets(pathset1 as (n1:int, paths1)::rest1, 
                      pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then 
        case intersectPathLists(paths1, paths2)
          of nil => intersectPathsets(rest1, rest2)
           | pl => (n1, pl)::(intersectPathsets(rest1, rest2))
      else if n1 < n2 then 
        intersectPathsets(rest1, pathset2)
      else
        intersectPathsets(pathset1, rest2)
  
fun unitePathsets(pathset1, nil) = pathset1
  | unitePathsets(nil, pathset2) = pathset2
  | unitePathsets(pathset1 as (n1:int, paths1)::rest1, 
                  pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then 
        (n1, unitePathLists(paths1, paths2))
          :: (unitePathsets(rest1, rest2))
      else if n1 < n2 then 
        (n1, paths1)::(unitePathsets(rest1, pathset2))
      else
        (n2, paths2)::(unitePathsets(pathset1, rest2))
  
fun differencePathsets(pathset1, nil) = pathset1
  | differencePathsets(nil, pathset2) = nil
  | differencePathsets(pathset1 as (n1:int, paths1)::rest1, 
                       pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then 
        case differencePathLists(paths1, paths2)
          of nil => differencePathsets(rest1, rest2)
           | pl => (n1, pl)::(differencePathsets(rest1, rest2))
      else if n1 < n2 then 
        (n1, paths1)::(differencePathsets(rest1, pathset2))
      else
        differencePathsets(pathset1, rest2)

fun doPathsetMember(path, metric, (n:int, paths)::rest) =
      (n < metric andalso doPathsetMember(path, metric, rest))
        orelse (n = metric andalso onPathList(path, paths))
  | doPathsetMember(path, metric, nil) = false    

fun doAddElementToPathset(path, metric, nil) = [(metric, [path])]
  | doAddElementToPathset(path, metric, (n:int, paths)::rest) =
      if n = metric then (n, addPathToPathList(path, paths))::rest
      else if n < metric then
        (n,paths)::(doAddElementToPathset(path, metric, rest))
      else (metric, [path])::(n, paths)::rest

fun dividePathset(pred, nil) = (nil, nil)
  | dividePathset(pred, (n, pathlist)::rest) =
      let val (yesSet, noSet) = dividePathset(pred, rest)
      in case dividePathList(pred, pathlist, nil, nil) 
           of (nil, nil) => bug "paths dissappeared during divide"
            | (nil, no) => (yesSet, (n,no)::noSet)
            | (yes, nil) => ((n, yes)::yesSet, noSet)
            | (yes, no) => ((n, yes)::yesSet, (n,no)::noSet)
      end

fun pathDepends path1 ROOTPATH = pathEq(path1, ROOTPATH)
  | pathDepends path1 (path2 as RECORDPATH paths) = 
      foldl (fn (a, b) => (pathDepends path1 a) orelse b) 
              (pathEq(path1, path2)) paths 
  | pathDepends path1 (path2 as PIPATH(_, subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath      
  | pathDepends path1 (path2 as VPIPATH(_,_,subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath      
  | pathDepends path1 (path2 as DELTAPATH(_,subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath
  | pathDepends path1 (path2 as (VLENPATH (subpath, _))) =
      pathEq(path1, path2) orelse pathDepends path1 subpath

fun pathMetric (ROOTPATH) = 0
  | pathMetric (RECORDPATH paths) =
      foldr (fn (a, b) => pathMetric a + b) 1 paths
  | pathMetric (PIPATH(_, subpath)) =
      1 + pathMetric subpath
  | pathMetric (VPIPATH(_,_,subpath)) =
      1 + pathMetric subpath
  | pathMetric (DELTAPATH(_,subpath)) =
      1 + pathMetric subpath
  | pathMetric (VLENPATH (subpath, _)) =
      1 + pathMetric subpath

fun pathsetMember path pathset = 
      doPathsetMember(path, pathMetric path, pathset)

fun addPathToPathset(path, pathset) =
      doAddElementToPathset(path, pathMetric path, pathset) 


fun doDoBindings(nil, rhs) = rhs
  | doDoBindings(path::rest, rhs) = BIND(path, doDoBindings(rest, rhs))

fun doBindings (nil, rhs) = rhs
  | doBindings ((n,paths)::morepaths, rhs) = 
         doDoBindings(paths, doBindings(morepaths, rhs))

fun subPaths (ROOTPATH) = [(0, [ROOTPATH])]
  | subPaths (path as RECORDPATH paths) =
         foldr unitePathsets [(pathMetric path, [path])] (map subPaths paths)
  | subPaths (path as (VLENPATH (subpath, _))) =
         (subPaths subpath)@[(pathMetric path, [path])]
  | subPaths (path as VPIPATH (n,_,subpath)) =
         (subPaths subpath)@[(pathMetric path, [path])]
  | subPaths (path as PIPATH (n, subpath)) =
         (subPaths subpath)@[(pathMetric path, [path])]
  | subPaths (path as DELTAPATH (_,subpath)) =
         (subPaths subpath)@[(pathMetric path, [path])]

fun rhsbindings (n, ruleDesc) = 
     let val (_, paths, _) = List.nth(ruleDesc, n)
      in foldr unitePathsets [] (map subPaths paths)
     end

fun pass1cases((pcon,subtree)::rest, envin, SOME envout, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(DELTAPATH(pcon,path)),myEnvout)
            val envoutSoFar = intersectPathsets(envout, otherBindings)
            val (rest', envout') = 
                  pass1cases(rest, envin, SOME envoutSoFar, rhs, path)
            val iBind2 = differencePathsets(otherBindings, envout')
            val subtree'' = 
                  doBindings(unitePathsets(mustBindHere, iBind2), subtree')
         in ((pcon,subtree'')::rest', envout')
        end
  | pass1cases((pcon,subtree)::rest, envin, NONE, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(DELTAPATH(pcon,path)),myEnvout)
            val (rest', envout') = 
                  pass1cases(rest, envin, SOME otherBindings, rhs, path)
            val iBind2 = differencePathsets(otherBindings, envout')
            val subtree'' = 
                  doBindings(unitePathsets(mustBindHere, iBind2), subtree')
         in ((pcon,subtree'')::rest', envout')
        end
  | pass1cases(nil, envin, SOME envout, rhs, path) =
        (nil, unitePathsets(envin, envout))
  | pass1cases(nil, envin, NONE, rhs, path) = bug "pass1cases bad"

and pass1(RHS n, envin, rhs) = (RHS n, rhsbindings(n, rhs))
  | pass1(CASETEST(path, sign, cases, NONE), envin, rhs) =        
        let val (cases', envout') =
              pass1cases(cases, unitePathsets(envin, subPaths path), 
                         NONE, rhs, path)
         in (CASETEST(path, sign, cases', NONE), envout')
        end
  | pass1(CASETEST(path, sign, cases, SOME subtree), envin, rhs) =        
        let val newenv = unitePathsets(envin, subPaths path)
            val (subtree', subEnvout) = pass1(subtree, newenv, rhs)
            val (cases', envout') =
              pass1cases(cases, newenv, SOME subEnvout, rhs, path)
            val subbindings = differencePathsets(subEnvout, envout')
            val subtree'' = doBindings(subbindings, subtree')
         in (CASETEST(path, sign, cases', SOME subtree''), envout')
        end
  | pass1 (ABSTEST0(path, con, subtree1, subtree2), envin, rhs) =
        let val newenv = unitePathsets(envin, subPaths path)
            val (subtree1', subEnvout1) = pass1(subtree1, newenv, rhs)
            val (subtree2', subEnvout2) = pass1(subtree2, newenv, rhs)
            val envout =
              unitePathsets(newenv,intersectPathsets(subEnvout1,subEnvout2))
            val bind1 = differencePathsets(subEnvout1, envout)
            val bind2 = differencePathsets(subEnvout2, envout)
            val subtree1'' = doBindings(bind1, subtree1')
            val subtree2'' = doBindings(bind2, subtree2')
         in (ABSTEST0(path, con, subtree1'', subtree2''), envout)
        end
  | pass1 (ABSTEST1(path, con, subtree1, subtree2), envin, rhs) =
        let val newenv = unitePathsets(envin, subPaths path)
            val yesenv =
                 if isAnException con then newenv
                 else addPathToPathset(DELTAPATH(DATApcon con, path), envin)
            val (subtree1', subEnvout1) = pass1(subtree1, yesenv, rhs)
            val (subtree2', subEnvout2) = pass1(subtree2, newenv, rhs)
           val envout = 
                  unitePathsets(newenv,
                                intersectPathsets(subEnvout1,subEnvout2))
            val bind1 = differencePathsets(subEnvout1, envout)
            val bind2 = differencePathsets(subEnvout2, envout)
            val subtree1'' = doBindings(bind1, subtree1')
            val subtree2'' = doBindings(bind2, subtree2')
         in (ABSTEST1(path, con, subtree1'', subtree2''), envout)
        end
  | pass1 _ = bug "pass1 bad"


(* 
 * Given a decision tree for a match, a list of ?? and the name of the 
 * variable bound to the value to be matched, produce code for the match. 
 *)
fun generate (dt, matchRep, rootVar, (toTyc, toLty), giis) = 
  let val (subtree, envout) = pass1(dt, [(0, [ROOTPATH])], matchRep)
      fun mkDcon (DATACON {name, rep, typ, ...}) = 
            (name, rep, toDconLty toLty typ)
      fun genpath (RECORDPATH paths, env) =
            RECORD (map (fn path => VAR(lookupPath (path, env))) paths)
        | genpath (PIPATH(n, path), env) = 
            SELECT(n, VAR(lookupPath(path, env)))
        | genpath (p as DELTAPATH(pcon, path), env) = 
            VAR(lookupPath(p, env))
        | genpath (VPIPATH(n, t, path), env) =
            let val tc = toTyc t
                val lt_sub = 
                  let val x = LT.ltc_vector (LT.ltc_tv 0)
                   in LT.ltc_poly([LT.tkc_mono], 
                    [LT.ltc_parrow(LT.ltc_tuple [x, LT.ltc_int], LT.ltc_tv 0)])
                  end
             in APP(PRIM(PO.SUBSCRIPTV, lt_sub, [tc]),
                    RECORD[VAR(lookupPath(path, env)), INT n])
            end
        | genpath (VLENPATH (path, t), env) = 
            let val tc = toTyc t
                val lt_len = LT.ltc_poly([LT.tkc_mono], 
                                 [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
                val argtc = LT.tcc_vector tc
             in APP(PRIM(PO.LENGTH, lt_len, [argtc]), 
                    VAR(lookupPath(path, env)))
            end
        | genpath (ROOTPATH, env) = VAR(lookupPath(ROOTPATH, env))

      fun genswitch(sv, sign, [(DATAcon((_, DA.REF, lt), ts, x), e)], NONE) = 
            LET(x, APP (PRIM (PrimOp.DEREF, LT.lt_swap lt, ts), sv), e)
        | genswitch(sv, sign, [(DATAcon((_, DA.SUSP(SOME(_, DA.LVAR f)), lt),
                                        ts, x), e)], NONE) = 
            let val v = mkv()
             in LET(x, LET(v, TAPP(VAR f, ts), APP(VAR v, sv)), e)
            end
	| genswitch (sv, sign, cases as ((INTINFcon _, _) :: _), default) =
	    let fun strip (INTINFcon n, e) = (n, e)
		  | strip _ = bug "genswitch: INTINFcon"
	    in
		case default of
		    NONE => bug "no default in switch on INTINF"
		  | SOME d => giis (sv, map strip cases, d)
	    end
        | genswitch x = SWITCH x

      fun pass2rhs (n, env, ruleDesc) = 
        (case List.nth(ruleDesc, n)
          of (_, [path], fname) => APP(VAR fname, VAR(lookupPath(path, env)))
           | (_, paths, fname) =>
               APP(VAR fname, 
                 RECORD (map (fn path => VAR(lookupPath(path, env))) paths)))

      fun pass2 (BIND(DELTAPATH _, subtree), env, rhs) = 
            pass2(subtree, env, rhs)
            (** we no longer generate explicit DECON anymore, instead,
                we add a binding at each switch case. *)
        | pass2 (BIND(path, subtree), env, rhs) =
            let val newvar = mkv()
                val subcode = pass2(subtree, (path, newvar)::env, rhs)
             in LET(newvar, genpath(path, env), subcode)
            end
        | pass2 (CASETEST(path, sign, [], NONE), _, _) = 
            bug "unexpected empty cases in matchcomp"
        | pass2 (CASETEST(path, sign, [], SOME subtree), env, rhs) = 
            pass2(subtree,env,rhs)
        | pass2 (CASETEST(path, sign, cases, dft), env, rhs) = 
            let val sv = VAR(lookupPath(path, env))
             in genswitch(sv, sign, pass2cases(path,cases,env,rhs), 
                          (case dft 
                            of NONE => NONE
                             | SOME subtree => SOME(pass2(subtree,env,rhs))))
            end
        | pass2 (ABSTEST0(path, con as (dc, _), yes, no), env, rhs) =
(*          if isAnException con 
            then genswitch(VAR(lookupPath(path, env)), DA.CNIL, 
                           [(DATAcon(mkDcon dc),  pass2(yes, env, rhs))],
                           SOME(pass2(no, env, rhs)))
            else *) 
            abstest0(path, con, pass2(yes,env,rhs), pass2(no,env,rhs)) 
        | pass2 (ABSTEST1(path, con as (dc, _), yes, no), env, rhs) =
(*          if isAnException con 
            then genswitch(VAR(lookupPath(path, env)), DA.CNIL,
                           [(DATAcon(mkDcon dc),  pass2(yes, env, rhs))],
                           SOME(pass2(no, env, rhs)))
            else *)
            abstest1(path, con, pass2(yes,env,rhs), pass2(no,env,rhs)) 
        | pass2 (RHS n, env, rhs) = pass2rhs(n, env, rhs)  

      and pass2cases(path, nil, env, rhs) = nil
        | pass2cases(path, (pcon,subtree)::rest, env, rhs) = 
            let (** always implicitly bind a new variable at each branch. *)
                val (ncon, nenv) = pconToCon(pcon, path, env)
                val res = (ncon, pass2(subtree, nenv, rhs))
             in res::(pass2cases(path, rest, env, rhs))
            end

      and pconToCon(pcon, path, env) =
        (case pcon
          of DATApcon (dc, ts) => 
               let val newvar = mkv()
                   val nts = map (toTyc o TP.VARty) ts
                   val nenv = (DELTAPATH(pcon, path), newvar)::env
                in (DATAcon (mkDcon dc, nts, newvar), nenv)
               end
           | VLENpcon(i, t) => (VLENcon i, env)
           | INTpcon i => (INTcon i, env)
           | INT32pcon i => (INT32con i, env)
	   | INTINFpcon n => (INTINFcon n, env)
           | WORDpcon w => (WORDcon w, env)
           | WORD32pcon w => (WORD32con w, env)
           | REALpcon r => (REALcon r, env)
           | STRINGpcon s => (STRINGcon s, env))

   in case doBindings(envout, subtree)
       of BIND(ROOTPATH, subtree') => 
            pass2(subtree', [(ROOTPATH, rootVar)], matchRep)
        | _ => pass2(subtree, [], matchRep)
  end
 
fun doMatchCompile(rules, finish, rootvar, toTcLt as (_, toLty), err, giis) =
  let val lastRule = length rules - 1
      val matchReps = map (preProcessPat toLty) rules
      val (matchRep,rhsRep) = 
        foldr (fn ((a,b),(c,d)) => (a@c,b::d)) ([], []) matchReps

      val allRules = mkAllRules(matchRep,0)
      val flattened = flattenAndors(makeAndor(matchRep,err),allRules)
      val ready = fireConstraint(ROOTPATH,flattened,nil,nil)
      val dt = genDecisionTree(ready,allRules)
      val numRules = length matchRep
      val rawUnusedRules = complement(0,numRules,rulesUsed dt)
      val unusedRules = rev(fixupUnused(rawUnusedRules,matchReps,0,0,nil))
      val exhaustive = isthere(lastRule,unusedRules)
      val redundantF = redundant(unusedRules, lastRule)

      fun g((fname, fbody), body) = LET(fname, fbody, body)
      val code = foldr g (generate(dt, matchRep, rootvar, toTcLt,giis)) rhsRep

   in (finish(code), unusedRules, redundantF, exhaustive)
  end

(*
 * Test pat, the guard pattern of the first match rule of a match,
 * for the occurence of variables (including layering variables) 
 * or wildcards.  Return true if any are present, false otherwise.
 *)      
fun noVarsIn ((pat,_)::_) =
      let fun var WILDpat = true (* might want to flag this *)
            | var (VARpat _) = true
            | var (LAYEREDpat _) = true
            | var (CONSTRAINTpat(p,_)) = var p
            | var (APPpat(_,_,p)) = var p
            | var (RECORDpat{fields,...}) = List.exists (var o #2) fields
            | var (VECTORpat(pats,_)) = List.exists var pats
            | var (ORpat (pat1,pat2)) = var pat1 orelse var pat2
            | var _ = false
       in not(var pat)
      end
  | noVarsIn _ = bug "noVarsIn in mc"


(* 
 * The three entry points for the match compiler.
 *
 * They take as arguments an environment (env); a match represented
 * as a list of pattern--lambda expression pairs (match); and a 
 * function to use in printing warning messages (warn).
 *
 * env and warn are only used in the printing of diagnostic information.
 *
 * If the control flag Control.MC.printArgs is set, they print match.  
 *  
 * They call doMatchCompile to actually compile match.
 * This returns a 4-tuple (code, unused, redundant, exhaustive).
 * code is lambda code that implements match.  unused
 * is a list of the indices of the unused rules.  redundant 
 * and exhaustive are boolean flags which are set if 
 * match is redundant or exhaustive respectively.
 *
 * They print warning messages as appropriate, as described below.
 * If the control flag Control.MC.printRet is set, they print code.
 *
 * They return code.
 *
 * They assume that match has one element for each rule of the match 
 * to be compiled, in order, plus a single, additional, final element.
 * This element must have a pattern that is always matched 
 * (in practice, it is either a variable or wildcard), and a
 * lambda expression that implements the appropriate behavior 
 * for argument values that satisfy none of the guard patterns.
 * A pattern is exhaustive if this dummy rule is never used,
 * and is irredundant if all of the other rules are used.
 *)   

local open Control.MC (* make various control flags visible *)
in      

(* 
 * Entry point for compiling matches induced by val declarations
 * (e.g., val listHead::listTail = list).  match is a two 
 * element list.  If the control flag Control.MC.bindNonExhaustiveWarn
 * is set, and match is nonexhaustive a warning is printed.  If the control
 * flag Control.MC.bindNoVariableWarn is set, and the first pattern
 * (i.e., the only non-dummy pattern) of match contains no variables or 
 * wildcards, a warning is printed.    Arguably, a pattern containing no 
 * variables, but one or more wildcards, should also trigger a warning, 
 * but this would cause warnings on constructions like
 * val _ = <exp>  and  val _:<ty> = <exp>.
 *)
fun bindCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ = 
        if !printArgs then (say "MC called with:"; MP.printMatch env rules)
        else ()
      val (code, _, _, exhaustive) = 
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)

      val nonexhaustiveF =
	  not exhaustive andalso
	  (!bindNonExhaustiveWarn orelse !bindNonExhaustiveError)
      val noVarsF = !bindNoVariableWarn andalso noVarsIn rules

   in if nonexhaustiveF
      then err (if !bindNonExhaustiveError then EM.COMPLAIN else EM.WARN)
	       ("binding not exhaustive" ^
	                (if noVarsF then " and contains no variables" else ""))
		       (bindPrint(env,rules))
      else if noVarsF
           then err EM.WARN "binding contains no variables" 
                    (bindPrint(env,rules))
           else ();

      if !printRet then 
        (say "MC:  returns with\n"; MP.printLexp code)
      else ();
      code
  end

(* 
 * Entry point for compiling matches induced by exception handlers.
 * (e.g., handle Bind => Foo).  If the control flag 
 *  Control.MC.matchRedundantWarn is set, and match is redundant, 
 *  a warning is printed.  If Control.MC.matchRedundantError is also
 *  set, the warning is promoted to an error message.
 *)
fun handCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ = 
        if !printArgs then (say "MC called with: "; MP.printMatch env rules)
        else ()
      val (code, unused, redundant, _) = 
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)
      val  redundantF= !matchRedundantWarn andalso redundant

   in if redundantF 
      then err 
	     (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	     "redundant patterns in match"
             (matchPrint(env,rules,unused))
      else ();

      if !printRet 
      then (say "MC:  returns with\n"; MP.printLexp code)
      else ();
      code
  end

(* 
 * Entry point for compiling matches induced by function expressions
 * (and thus case expression, if-then-else expressions, while expressions
 * and fun declarations) (e.g., fn (x::y) => ([x],y)). If the control flag 
 * Control.MC.matchRedundantWarn is set, and match is redundant, a warning 
 * is printed; if Control.MC.matchRedundantError is also set, the warning
 * is promoted to an error. If the control flag Control.MC.matchExhaustive
 * is set, and match is nonexhaustive, a warning is printed.   
 *)
fun matchCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ = 
        if !printArgs then (say "MC called with: "; MP.printMatch env rules)
        else ()
      val (code, unused, redundant, exhaustive) = 
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)

      val nonexhaustiveF = 
	  not exhaustive andalso
	  (!matchNonExhaustiveError orelse !matchNonExhaustiveWarn)
      val redundantF = 
	  redundant andalso (!matchRedundantError orelse !matchRedundantWarn)
   in case (nonexhaustiveF,redundantF)
       of (true, true) =>
            err (if !matchRedundantError orelse !matchNonExhaustiveError
		     then EM.COMPLAIN else EM.WARN)
	        "match redundant and nonexhaustive"
	        (matchPrint(env, rules, unused))

        | (true, false) =>
            err (if !matchNonExhaustiveError then EM.COMPLAIN else EM.WARN)
                "match nonexhaustive"
		(matchPrint(env, rules, unused))

        | (false, true) =>
            err (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	      "match redundant" (matchPrint(env, rules, unused))

        | _ => ();

      if (!printRet) 
      then (say "MatchComp:  returns with\n"; MP.printLexp code) else ();
      code
  end


val matchCompile = 
  Stats.doPhase(Stats.makePhase "Compiler 045 matchcomp") matchCompile

end (* local Control.MC *)

end (* topleve local *)
end (* structure MatchComp *)

