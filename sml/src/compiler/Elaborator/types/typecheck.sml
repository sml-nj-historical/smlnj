(* Copyright 1996 by Bell Laboratories *)
(* typecheck.sml *)

signature TYPECHECK = 
sig

  val decType : StaticEnv.staticEnv * Absyn.dec * bool
		* ErrorMsg.errorFn * SourceMap.region -> Absyn.dec
  val debugging : bool ref

end (* signature TYPECHECK *)


(* functorized to factor out dependencies on FLINT... *)
functor TypecheckFn (val ii_ispure : II.ii -> bool
	     (* PRIMOP: val ii2ty : II.ii -> Types.ty option *)) : TYPECHECK =
struct

local open Array List Types VarCon BasicTypes TypesUtil Unify Absyn
           Overload ErrorMsg PrettyPrint PPUtil PPType PPAbsyn

  structure SE = StaticEnv
  (* structure II = InlInfo *)
  structure DA = Access
  structure EU = ElabUtil
  structure ED = ElabDebug
  structure OLL = OverloadLit
  structure PP = PrettyPrint
	  
in 

(* debugging *)
val say = Control_Print.say
val debugging = ref false
fun debugmsg (msg: string) = if !debugging then (say msg; say "\n") else ()
val debugPrint = (fn x => ED.debugPrint debugging x)

fun bug msg = ErrorMsg.impossible("TypeCheck: "^msg)

val isValue = isValue { ii_ispure = ii_ispure }

infix 9 sub
infix -->

val printDepth = Control_Print.printDepth

fun refNewDcon(DATACON{name,const,rep,typ,sign,lazyp}) = 
  DATACON{name=name,const=const,rep=rep,typ=refPatType,sign=sign,lazyp=lazyp}

exception NotThere

fun message(msg,mode: Unify.unifyFail) =
    String.concat[msg," [",Unify.failMessage mode,"]"]

fun mkDummy0 () = BasicTypes.unitTy

(*
 * decType : SE.staticEnv * A.dec * bool * EM.errorFn * region -> A.dec 
 *)
fun decType(env,dec,toplev,err,region) = 
let

val ppType = PPType.ppType env
val ppPat = PPAbsyn.ppPat env
val ppExp = PPAbsyn.ppExp(env,NONE)
val ppRule = PPAbsyn.ppRule(env,NONE)
val ppVB = PPAbsyn.ppVB(env,NONE)
val ppRVB = PPAbsyn.ppRVB(env,NONE)
val ppDec = 
  (fn ppstrm => fn d => PPAbsyn.ppDec (env,NONE) ppstrm (d,!printDepth))

fun ppDecDebug (msg,dec) =
  ED.withInternals(fn () => ED.debugPrint debugging (msg, ppDec, dec))

fun ppTypeDebug (msg,ty) =
  ED.withInternals(fn () => ED.debugPrint debugging (msg, ppType, ty))

fun ppTyvarDebug tv = 
  ED.withInternals(fn () => debugmsg (PPType.tyvarPrintname tv))

fun unifyErr{ty1,name1,ty2,name2,message=m,region,kind,kindname,phrase} =
    (unifyTy(ty1,ty2); true) handle Unify(mode) =>
      (err region COMPLAIN (message(m,mode))
       (fn ppstrm => 
	 (PPType.resetPPType();
	  let val len1= size name1 
	      val len2= size name2
	      val spaces = "                                   "
	      val pad1= substring(spaces,0,Int.max(0,len2-len1))
	      val pad2= substring(spaces,0,Int.max(0,len2-len1))
	      val m = if m="" then name1 ^ " and " ^ name2 ^ " don't agree"
		      else m
	  in if name1="" then ()
             else (newline ppstrm; 
                   PP.string ppstrm (name1 ^ ": " ^ pad1);
	           ppType ppstrm ty1); 
	     if name2="" then ()
	      else (newline ppstrm; 
                    PP.string ppstrm (name2 ^ ": " ^ pad2);
		    ppType ppstrm ty2);
	     if kindname="" then ()
	     else (newline ppstrm; PP.string ppstrm("in "^kindname^":");
		   break ppstrm {nsp=1,offset=2}; kind ppstrm (phrase,!printDepth))
	 end));
       false)

val _ = debugmsg (">>decType: toplev = " ^ Bool.toString toplev)
val _ = ppDecDebug(">>decType: dec = ",dec)

fun generalizeTy(VALvar{typ,path,...}, userbound: tyvar list,
		 occ:occ, generalize: bool, region) : tyvar list =
    let val _ = debugmsg ("generalizeTy: "^SymPath.toString path)
	val _ = debugmsg ("userbound: ")
	val _ = List.app ppTyvarDebug userbound

	val failure = ref false
	val mkDummy = if toplevel occ
	              then TypesUtil.dummyTyGen()
		      else mkDummy0 (* shouldn't be called *)

	val index = ref 0  (* counts no of type variables bound *)
	fun next() = let val i = !index in index := i+1; i end
	val sign = ref([]: Types.polysign)
	fun localUbound tv =
	    let fun mem(tv'::rest) = eqTyvar(tv,tv') orelse mem rest
		  | mem [] = false
	     in mem userbound
	    end
	(* menv: a reference to an association list environment mapping
	 *   generalized tyvars to the corresponding IBOUND type. 
	 * ASSERT: there are no duplicate tyvars in domain of menv. *)
	val menv = ref([]: (tyvar*ty) list)
	fun lookup tv =
	    let fun find [] = raise NotThere
		  | find((tv',ty)::rest) = if eqTyvar(tv,tv') then ty 
							      else find rest
	     in find(!menv)
	    end
	fun bind(tv,ty) = menv := (tv,ty) :: !menv
	fun gen(ty) =     
	    case ty
	     of VARty(ref(INSTANTIATED ty)) => gen ty
	      | VARty(tv as ref(OPEN{depth,eq,kind})) =>
		  (case kind
		     of FLEX[(lab,_)] =>
                         if ((depth > lamdepth occ) andalso
                             (generalize orelse (toplevel occ)))
                            orelse ((toplevel occ) andalso (depth=0))
                         then
			   (err region COMPLAIN (String.concat
			     ["unresolved flex record\n\
			      \   (can't tell what fields there are besides #",
			      Symbol.name lab, ")"])
			    nullErrorBody;
			   WILDCARDty)
                         else ty
		      | FLEX _ =>
                         if ((depth > lamdepth occ) andalso
                             (generalize orelse (toplevel occ)))
                            orelse ((toplevel occ) andalso (depth=0))
                         then
  			   (err region COMPLAIN 
			        "unresolved flex record (need to know the \
			        \names of ALL the fields\n in this context)"
			    (fn ppstrm =>
			       (PPType.resetPPType();
				newline ppstrm;
				PP.string ppstrm "type: ";
				ppType ppstrm ty));
			    WILDCARDty)
                         else ty
		      | META =>
			  if depth > lamdepth occ
			  then if generalize then
				  lookup tv handle NotThere =>
				    let val new = IBOUND(next())
				     in sign := eq :: !sign;
				        bind(tv,new); 
					new
				    end
			       else (if toplevel occ
				     then let val new = mkDummy()
					   in failure := true;
                                              tv := INSTANTIATED new;
					      new
					  end
				     else (if !ElabControl.valueRestrictionLocalWarn
					   then err region WARN
				             ("type variable not generalized\
                                              \ in local decl (value restriction): "
                                              ^ (tyvarPrintname tv))
				             nullErrorBody
					   else ();
					   (* reset depth to prevent later
					      incorrect generalization inside
					      a lambda expression.  See typechecking
					      test 5.sml *)
					   tv := OPEN{depth = lamdepth occ,
						      eq = eq, kind = kind};
					   ty))
			  else if toplevel occ andalso depth = 0
			   (* ASSERT: failed generalization at depth 0.
			      see bug 1066. *)
			    then lookup tv handle NotThere =>
				 let val new = mkDummy()
				  in failure := true;
                                     tv := INSTANTIATED new;
				     new
				 end
			  else ty) (* raise SHARE *)
	      | VARty(tv as ref(UBOUND{name,depth,eq})) =>
		 (debugmsg ("UBOUND:" ^Symbol.name name);
		  if localUbound tv
		  then (debugmsg "is local";
		       if depth > lamdepth occ andalso generalize
		       then (debugmsg "is generalized";
			     lookup tv handle NotThere =>
			      let val new = IBOUND(next())
			       in sign := eq :: !sign;
				  bind(tv,new);
				  new
			      end)
		       else (err region COMPLAIN
			     ("explicit type variable cannot be \
			       \generalized at its binding \
			       \declaration: " ^
			       (tyvarPrintname tv))
			      nullErrorBody;
			     tv := INSTANTIATED WILDCARDty;
			     WILDCARDty))
		  else (debugmsg "is not local"; ty))
	      | (VARty(ref(LITERAL _)) | VARty(ref(SCHEME _))) => ty
	      | CONty(tyc,args) => CONty(tyc, map gen args) (*shareMap*)
	      | WILDCARDty => WILDCARDty
	      | _ => bug "generalizeTy -- bad arg"

	val _ = ppTypeDebug (">>gen: before: ",!typ)
	val ty = gen(!typ)
	val _ = ppTypeDebug (">>gen: after: ",ty)

        val generalizedTyvars = map #1 (rev(!menv))

        (* a hack to eliminate all user bound type variables --zsh *)
	(* ZHONG?: is this still necessary? [dbm] *)
	fun elimUbound(tv as ref(UBOUND{depth,eq,...})) = 
              (tv := OPEN{depth=depth,eq=eq,kind=META})
          | elimUbound _ = ()

        (* turn ubound tyvars into ordinary META tyvars *)
        val _ = app elimUbound generalizedTyvars

     in if !failure andalso !ElabControl.valueRestrictionTopWarn
	  then err region WARN
	        "type vars not generalized because of\n\
                 \   value restriction are instantiated to dummy types (X1,X2,...)"
		nullErrorBody
          else ();
	debugmsg "generalizeTy returning";
	typ := POLYty{sign = rev(!sign),
		      tyfun = TYFUN{arity=(!index),body=ty}};
	generalizedTyvars  (* return the tyvars that were generalized *)
    end

  | generalizeTy _ = bug "generlizeTy - bad arg"
  

fun generalizePat(pat: pat, userbound: tyvar list, occ: occ, 
                  generalize: bool, region) =
    let val tvs : tyvar list ref = ref []
        fun gen(VARpat v) = 
	      (let val x = generalizeTy(v,userbound,occ,generalize,region)
                   val _ = case (x, !tvs) 
                            of (_::_, _::_) => bug "generalizePat 1234"
                             | _ => ()
                in tvs := (x@(!tvs))
               end)
	  | gen(RECORDpat{fields,...}) = app (gen o #2) fields
	  | gen(APPpat(_,_,arg)) = gen arg
	  | gen(CONSTRAINTpat(pat,_)) = gen pat
	  | gen(LAYEREDpat(varPat,pat)) = (gen varPat; gen pat)
	  | gen _ = ()
     in gen pat; !tvs
    end

fun applyType(ratorTy: ty, randTy: ty) : ty =
  let val resultType = mkMETAty()
   in unifyTy(ratorTy, (randTy --> resultType)); resultType
  end

fun patType(pat: pat, depth, region) : pat * ty =
    case pat
      of WILDpat => (pat,mkMETAtyBounded depth)
       | VARpat(VALvar{typ as ref UNDEFty,...}) => 
	      (typ := mkMETAtyBounded depth; (pat,!typ))
			             (* multiple occurrence due to or-pat *)
       | VARpat(VALvar{typ, ...}) => (pat, !typ) 
       | INTpat (_,ty) => (OLL.push ty; (pat,ty))
       | WORDpat (_,ty) => (OLL.push ty; (pat,ty))
       | REALpat _ => (pat,realTy)
       | STRINGpat _ => (pat,stringTy)
       | CHARpat _ => (pat,charTy)
       | RECORDpat{fields,flex,typ} =>
	   (* fields assumed already sorted by label *)
	   let fun g(lab,pat') = 
                 let val (npat,nty) = patType(pat',depth,region)
                  in ((lab,npat), (lab,nty))
                 end
               val (fields',labtys) = mapUnZip g fields
               val npat = RECORDpat{fields=fields',flex=flex,typ=typ}
	    in if flex
	       then let val ty = VARty(mkTyvar(mkFLEX(labtys,depth)))
		     in typ := ty; (npat,ty)
		    end
	       else (npat,recordTy(labtys))
	   end
       | VECTORpat(pats,_) => 
          (let val (npats,ntys) = 
                     mapUnZip (fn pat => patType(pat,depth,region)) pats
               val nty =
	       foldr (fn (a,b) => (unifyTy(a,b); b)) (mkMETAtyBounded depth) ntys
            in (VECTORpat(npats,nty), CONty(vectorTycon,[nty]))
           end handle Unify(mode) => (
	     err region COMPLAIN 
		 (message("vector pattern type failure",mode)) nullErrorBody;
	     (pat,WILDCARDty)))
       | ORpat(p1, p2) => 
           let val (p1, ty1) = patType(p1, depth, region)
  	       val (p2, ty2) = patType(p2, depth, region)
	   in
	     unifyErr{ty1=ty1,ty2=ty2,name1="expected",name2="found",
		      message="or-patterns don't agree",region=region,
		      kind=ppPat,kindname="pattern",phrase=pat};
	     (ORpat(p1, p2), ty1)
	   end
       | CONpat(dcon as DATACON{typ,...},_) => 
           let val (ty, insts) = instantiatePoly typ
               (* the following is to set the correct depth information
                * to the type variables in ty. (ZHONG)
                *)
               val nty = mkMETAtyBounded depth
               val _ = unifyTy(nty, ty) 
            in (CONpat(dcon,insts),ty)
           end
       | APPpat(dcon as DATACON{typ,rep,...},_,arg) =>
	   let val (argpat,argty) = patType(arg,depth,region)
               val (ty1,ndcon) = case rep
                                  of DA.REF => (refPatType,refNewDcon dcon)
                                   | _ => (typ,dcon)
               val (ty2,insts) = instantiatePoly ty1
               val npat = APPpat(ndcon,insts,argpat)
            in (npat,applyType(ty2,argty))
	       handle Unify(mode) =>
		(err region COMPLAIN
                  (message("constructor and argument don't agree in pattern",mode))
		  (fn ppstrm =>
		   (PPType.resetPPType();
		    newline ppstrm;
		    PP.string ppstrm "constructor: ";
		    ppType ppstrm typ; newline ppstrm;
		    PP.string ppstrm "argument:    ";
		    ppType ppstrm argty; newline ppstrm;
		    PP.string ppstrm "in pattern:"; break ppstrm {nsp=1,offset=2};
		    ppPat ppstrm (pat,!printDepth)));
		 (pat,WILDCARDty))
	   end
       | CONSTRAINTpat(pat',ty) => 
	   let val (npat,patTy) = patType(pat',depth,region)
	    in if unifyErr{ty1=patTy,name1="pattern",ty2=ty,name2="constraint",
			message="pattern and constraint don't agree",
			region=region,kind=ppPat,kindname="pattern",phrase=pat}
		then (CONSTRAINTpat(npat,ty),ty)
		else (pat,WILDCARDty)
	   end
       | LAYEREDpat(vpat as VARpat(VALvar{typ,...}),pat') =>
           let val (npat,patTy) = patType(pat',depth,region)
               val _ = (typ := patTy)
            in (LAYEREDpat(vpat,npat),patTy)
           end
       | LAYEREDpat(cpat as CONSTRAINTpat(VARpat(VALvar{typ,...}),ty),pat') =>
	   let val (npat,patTy) = patType(pat',depth,region)
	    in if unifyErr{ty1=patTy,name1="pattern",ty2=ty,name2="constraint",
			   message="pattern and constraint don't agree",
			   region=region,kind=ppPat,kindname="pattern",phrase=pat}
		   then (typ := ty; (LAYEREDpat(cpat,npat),ty))
		  else (pat,WILDCARDty)
	   end
       | p => bug "patType -- unexpected pattern"

fun expType(exp: exp, occ: occ, region) : exp * ty =
let fun boolUnifyErr { ty, name, message } =
	unifyErr { ty1 = ty, name1 = name, ty2 = boolTy, name2 = "",
		   message = message, region = region, kind = ppExp,
		   kindname = "expression", phrase = exp }
    fun boolshortcut (con, what, e1, e2) =
	let val (e1', t1) = expType (e1, occ, region)
	    val (e2', t2) = expType (e2, occ, region)
	    val m = String.concat ["operand of ", what, " is not of type bool"]
	in
	    if boolUnifyErr { ty = t1, name = "operand", message = m }
	    andalso boolUnifyErr { ty = t2, name = "operand", message = m }
	    then (con (e1', e2'), boolTy)
	    else (exp, WILDCARDty)
	end
in
     case exp
      of VARexp(r as ref(VALvar{typ, ...}), _) =>
	  let val (ty, insts) = instantiatePoly(!typ)
	   in (VARexp(r, insts), ty)
	  end
(* PRIMOP: 
	 (case ii2ty info of
	      SOME st =>
              let val (sty, insts) = instantiatePoly(st)
		  val (nty, _) = instantiatePoly(!typ)
              in
		  unifyTy(sty, nty) handle _ => ();  (* ??? *)
		  (VARexp(r, insts), sty)
              end
	    | NONE =>
	      let val (ty, insts) = instantiatePoly(!typ)
	      in (VARexp(r, insts), ty)
	      end)
*)
       | VARexp(refvar as ref(OVLDvar _),_) =>
 	    (exp,pushOverloaded(refvar, err region))
       | VARexp(r as ref ERRORvar, _) => (exp, WILDCARDty)
       | CONexp(dcon as DATACON{typ,...},_) => 
           let val (ty,insts) = instantiatePoly typ
            in (CONexp(dcon,insts),ty)
           end
       | INTexp (_,ty) => (OLL.push ty; (exp,ty))
       | WORDexp (_,ty) => (OLL.push ty; (exp,ty))
       | REALexp _ => (exp,realTy)
       | STRINGexp _ => (exp,stringTy)
       | CHARexp _ => (exp,charTy)
       | RECORDexp fields =>
           let fun h(l as LABEL{name,...},exp') = 
                    let val (nexp,nty) = expType(exp',occ,region)
                     in ((l,nexp),(l,nty))
                    end
               fun extract(LABEL{name,...},t) = (name,t)
               val (fields',tfields) = mapUnZip h fields
               val rty = map extract (sortFields tfields)
            in (RECORDexp fields',recordTy(rty))
           end
       | SELECTexp (l, e) =>
           let val (nexp, nty) = expType(e, occ, region)
               val res = mkMETAty ()
               val labtys = [(EU.labsym l, res)]
               val pt = VARty(mkTyvar(mkFLEX(labtys,infinity)))
            in (unifyTy(pt,nty); (SELECTexp(l, nexp), res))
               handle Unify(mode) =>
                 (err region COMPLAIN
                  (message("selecting a non-existing field from a record",mode))
                  (fn ppstrm =>
                   (PPType.resetPPType();
                    newline ppstrm;
                    PP.string ppstrm "the field name: ";
                    (case l of LABEL{name,...} => ppSym ppstrm name);
                    newline ppstrm;
                    PP.string ppstrm "the record type:    ";
                    ppType ppstrm nty; newline ppstrm;
                    PP.string ppstrm "in expression:"; 
                    break ppstrm {nsp=1,offset=2};
                    ppExp ppstrm (exp,!printDepth)));
                    (exp, WILDCARDty))
           end
       | VECTORexp(exps,_) =>
          (let val (exps',nty) = mapUnZip (fn e => expType(e,occ,region)) exps
               val vty = foldr (fn (a,b) => (unifyTy(a,b); b)) (mkMETAty()) nty
            in (VECTORexp(exps',vty), CONty(vectorTycon,[vty]))
           end handle Unify(mode) =>
	   (err region COMPLAIN
	     (message("vector expression type failure",mode))
             nullErrorBody; (exp,WILDCARDty)))
       | SEQexp exps => 
	   let fun scan nil = (nil,unitTy)
	         | scan [e] = 
                     let val (e',ety) = expType(e,occ,region)
                      in ([e'],ety)
                     end
		 | scan (e::rest) = 
                     let val (e',_) = expType(e,occ,region)
                         val (el',ety) = scan rest
                      in (e'::el',ety)
                     end
               val (exps',expty) = scan exps
            in (SEQexp exps',expty)
	   end
       | APPexp(rator, rand) =>
	   let val (rator',ratorTy) = expType(rator,occ,region)
	       val (rand',randTy) = expType(rand,occ,region)
               val exp' = APPexp(rator',rand')
	    in (exp',applyType(ratorTy,randTy))
	       handle Unify(mode) => 
	       let val ratorTy = prune ratorTy
		   val reducedRatorTy = headReduceType ratorTy
		in PPType.resetPPType();
		   if isArrowType(reducedRatorTy)
		   then (err region COMPLAIN
			  (message("operator and operand don't agree",mode))
			  (fn ppstrm =>
			   (newline ppstrm;
			    PP.string ppstrm "operator domain: ";
			    ppType ppstrm (domain reducedRatorTy);
			    newline ppstrm;
			    PP.string ppstrm "operand:         ";
			    ppType ppstrm randTy; newline ppstrm;
			    PP.string ppstrm "in expression:";
			    break ppstrm {nsp=1,offset=2};
			    ppExp ppstrm (exp,!printDepth)));
			 (exp,WILDCARDty))
		   else (err region COMPLAIN
			  (message("operator is not a function",mode))
			  (fn ppstrm =>
			    (newline ppstrm;
			     PP.string ppstrm "operator: ";
			     ppType ppstrm (ratorTy); newline ppstrm;
			     PP.string ppstrm "in expression:";
			     break ppstrm {nsp=1,offset=2};
			     ppExp ppstrm (exp,!printDepth)));
			 (exp,WILDCARDty))
	       end
	   end
       | CONSTRAINTexp(e,ty) =>
	   let val (e',ety) = expType(e,occ,region)
	    in if unifyErr{ty1=ety,name1="expression", ty2=ty, name2="constraint",
			message="expression doesn't match constraint",
			region=region,kind=ppExp,kindname="expression",
			phrase=exp}
		then (CONSTRAINTexp(e',ty),ty)
		else (exp,WILDCARDty)
	   end
       | HANDLEexp(e,HANDLER h) =>
	   let val (e',ety) = expType(e,occ,region)
	       and (h',hty) = expType(h,occ,region)
               val exp' = HANDLEexp(e',HANDLER h')
	    in (unifyTy(hty, exnTy --> ety); (exp',ety))
	       handle Unify(mode) =>
		 (if unifyErr{ty1=domain(prune hty), name1="handler domain",
			     ty2=exnTy, name2="",
			     message="handler domain is not exn",
			     region=region,kind=ppExp,kindname="expression",
			     phrase=exp}
		     then unifyErr{ty1=ety, name1="body",
				   ty2=range(prune hty), name2="handler range",
				   message="expression and handler don't agree",
				   region=region,
				   kind=ppExp,kindname="expression",phrase=exp}
		     else false;
		  (exp,WILDCARDty))
	   end
       | RAISEexp(e,_) =>
	   let val (e',ety) = expType(e,occ,region)
               val newty = mkMETAty()
	    in unifyErr{ty1=ety, name1="raised", ty2=exnTy, name2="",
			message="argument of raise is not an exception",
			region=region,kind=ppExp,kindname="expression",phrase=exp};
	       (RAISEexp(e',newty),newty)
	   end
       | LETexp(d,e) => 
           let val d' = decType0(d,LetDef(occ),region)
               val (e',ety) = expType(e,occ,region)
            in (LETexp(d',e'),ety)
           end
       | CASEexp(e,rules,isMatch) =>
	   let val (e',ety) = expType(e,occ,region)
	       val (rules',_,rty) = matchType(rules,occ,region)
               val exp' = CASEexp(e',rules', isMatch)
	    in (exp',applyType(rty,ety))
	       handle Unify(mode) => 
	       (if isMatch then
		    unifyErr{ty1=domain rty, name1="rule domain",
			     ty2=ety, name2="object",
			     message="case object and rules don't agree",
			     region=region,kind=ppExp,kindname="expression",phrase=exp}
                else 
                 let val decl = case rules 
                                 of (RULE(pat,_))::_ => 
				    VB{pat=pat,exp=exp,tyvars=ref[],boundtvs=[]}
                                  | _ => bug "unexpected rule list 456"
		  in unifyErr{ty1=domain rty, name1="pattern",
			      ty2=ety, name2="expression",
			      message="pattern and expression in val dec don't agree",
			      region=region,kind=ppVB,kindname="declaration",
			      phrase=decl}
                 end;
	        (exp,WILDCARDty))
	   end
		 (* this causes case to behave differently from let, i.e.
		    bound variables do not have generic types *)
       | IFexp { test, thenCase, elseCase } =>
	   let val (test', tty) = expType (test, occ, region)
	       val (thenCase', tct) = expType (thenCase, occ, region)
	       val (elseCase', ect) = expType (elseCase, occ, region)
	   in
	       if boolUnifyErr
		      { ty = tty, name = "test expression",
			message="test expression in if is not of type bool" }
	       andalso
	          unifyErr { ty1 = tct, name1 = "then branch",
			     ty2 = ect, name2 = "else branch",
			     message="types of if branches do not agree",
			     region = region, kind = ppExp,
			     kindname = "expression", phrase = exp }
	       then
		   (IFexp { test = test', thenCase = thenCase',
			    elseCase = elseCase' },
		    tct)
	       else
		   (exp, WILDCARDty)
	   end
       | ANDALSOexp (e1, e2) =>
	   boolshortcut (ANDALSOexp, "andalso", e1, e2)
       | ORELSEexp (e1, e2) =>
	   boolshortcut (ORELSEexp, "orelse", e1, e2)
       | WHILEexp { test, expr } =>
	   let val (test', tty) = expType (test, occ, region)
	       val (expr', _) = expType (expr, occ, region)
	   in
	       if boolUnifyErr { ty = tty, name = "test expression",
				 message = "test expression in while is not of type bool" }
	       then
		   (WHILEexp { test = test', expr = expr' }, unitTy)
	       else
		   (exp, WILDCARDty)
	   end
       | FNexp(rules,_) => 
           let val (rules',ty,rty) = matchType(rules,occ,region)
            in (FNexp(rules',ty),rty)
           end
       | MARKexp(e,region) => 
           let val (e',et) = expType(e,occ,region)
            in (MARKexp(e',region),et)
           end
       | _ => bug "exptype -- bad expression"
end

and ruleType(RULE(pat,exp),occ,region) =  
 let val occ = Abstr occ
     val (pat',pty) = patType(pat,lamdepth occ,region)
     val (exp',ety) = expType(exp,occ,region)
  in (RULE(pat',exp'),pty,pty --> ety)
 end

and matchType(l,occ,region) =
    case l
      of [] => bug "empty rule list in typecheck.matchType"
       | [rule] => 
	    let val (rule0,argt,rty) = ruleType(rule,occ,region)
	     in ([rule0],argt,rty)
	    end
       | rule::rest =>
	    let val (rule0,argt,rty) = ruleType(rule,occ,region)
		fun checkrule rule' =
		   let val (rule1,argt',rty') = ruleType(rule',occ,region)
		    in unifyErr{ty1=rty,ty2=rty', name1="earlier rule(s)",
				name2="this rule",
				message="types of rules don't agree",
				region=region,
				kind=ppRule,kindname="rule",phrase=rule'};
		       rule1
		   end
	     in (rule0::(map checkrule rest),argt,rty)
	    end

and decType0(decl,occ,region) : dec =
     case decl
      of VALdec vbs =>
	   let fun vbType(vb as VB{pat, exp, tyvars=(tv as (ref tyvars)), boundtvs}) =
	        let val (pat',pty) = patType(pat,infinity,region)
		    val (exp',ety) = expType(exp,occ,region)
                    val generalize = isValue exp (* orelse isVarTy ety *)
		 in unifyErr{ty1=pty,ty2=ety, name1="pattern", name2="expression",
			     message="pattern and expression in val dec don't agree",
			     region=region,kind=ppVB,kindname="declaration",
			     phrase=vb};
                   VB{pat=pat',exp=exp',tyvars=tv,
                      boundtvs=generalizePat(pat,tyvars,occ,generalize,region)}
                end
	       val _ = debugmsg ">>decType0: VALdec"
	    in VALdec(map vbType vbs)
	   end

       | VALRECdec(rvbs) =>
 	   let val occ = Abstr occ

	       (* First go through and type-check all the patterns and
		  result-constraints, unifying with each other and with
		  the specified result type.
	       *)
	       fun setType(rvb as RVB{var=VALvar{typ,...},exp,resultty,...}) = 
                   let val domainty = mkMETAtyBounded(lamdepth occ)
		       val rangety = mkMETAtyBounded(lamdepth occ)

		       val funty = domainty --> rangety

		       val _ = 
			   case resultty 
			     of NONE => true
			      | SOME ty =>
				 unifyErr{ty1=funty,ty2=ty,
					  name1="",name2="constraint",
					  message="type constraint of val rec dec\
					           \ is not a function type",
					  region=region,kind=ppRVB,
					  kindname="declaration", phrase=rvb}

		       fun f(FNexp(rules,_), region, funty) =
		             let fun unify a =
				  (unifyErr{ty1=a,name1="this clause",
				    ty2=funty,name2="previous clauses",
				    message="parameter or result constraints\
			                     \ of clauses don't agree",
					   region=region,kind=ppRVB,
					   kindname="declaration", phrase=rvb};
                                  ())
				       
				 fun approxRuleTy(RULE(pat,e)) =
				     let val (pat',pty) =
					     patType(pat,lamdepth occ,region)
				      in case e
					  of CONSTRAINTexp(e,ty) =>
					      (pat',pty-->ty,(e,region))
					   | e => (pat',pty-->rangety,(e,region))
				     end

				 val patTyExps = map approxRuleTy rules
				 val pats = map #1 patTyExps
				 val tys = map #2 patTyExps
				 val exps = map #3 patTyExps

				 fun doExp (e,region) =
				     let val (exp', ety) = expType(e,occ,region)
				      in unifyErr{ty1=ety, name1="expression",
					  ty2=rangety, name2="result type",
					  message="right-hand-side of clause\
					\ doesn't agree with function result type",
					  region=region,kind=ppRVB,
					  kindname="declaration",phrase=rvb};
					 exp'
				     end

                              in app unify tys;
				 typ := funty;
				 fn()=> 
				   FNexp(ListPair.map RULE (pats, map doExp exps),
						domain(prune(funty)))
			     end
		         | f(MARKexp(e,region),_,funty) = 
			     let val build = f(e,region,funty)
			      in fn()=> MARKexp(build(), region)
			     end
                         | f(CONSTRAINTexp(e,ty),region,funty) =
			     let val _ = 
				   unifyErr{ty1=ty, name1="this constraint",
					    ty2=funty, name2="outer constraints",
					    message="type constraints on val rec\
					             \ declaraction disagree",
					    region=region,kind=ppRVB,
					    kindname="declaration", phrase=rvb}
				 val build = f(e,region,funty)
			     in fn()=> CONSTRAINTexp(build(), ty)
			    end
			| f _ = bug "typecheck.823"
                   in f(exp,region,funty)		      
                  end
		 | setType _ = bug "setType"

	      (* Second, go through and type-check the right-hand-side
	         expressions (function bodies) *)
	       fun rvbType(RVB{var=v,resultty,tyvars,boundtvs,...}, build) =
                      RVB{var=v,exp=build(), resultty=resultty,tyvars=tyvars,
			  boundtvs=boundtvs}
                  
	       val _ = debugmsg ">>decType0: VALRECdec"
               val builders = map setType rvbs
               val rvbs' = ListPair.map rvbType (rvbs,builders)
               (* No need to generalize here, because every VALRECdec is
                  wrapped in a VALdec, and the generalization occurs at the
                  outer level.  Previously: val rvbs'' = map genType rvbs' *)
	    in EU.recDecs rvbs'
	   end

       | EXCEPTIONdec(ebs) =>
	   let fun check(VARty(ref(UBOUND _))) = 
		     err region COMPLAIN
		         "type variable in top level exception type"
			 nullErrorBody
		 | check(CONty(_,args)) =
		     app check args
		 | check _ = ()
	       fun ebType(EBgen{etype=SOME ty,...}) = check ty
	         | ebType _ = ()
	       val _ = debugmsg ">>decType0: EXCEPTIONdec"
            in if TypesUtil.lamdepth occ < 1 then app ebType ebs else ();
               decl
	   end
       | LOCALdec(decIn,decOut) =>
	   let val decIn' = decType0(decIn,LetDef occ,region)
               val decOut' = decType0(decOut,occ,region)
	       val _ = debugmsg ">>decType0: LOCALdec"
            in LOCALdec(decIn',decOut')
           end
       | SEQdec(decls) => 
           SEQdec(map (fn decl => decType0(decl,occ,region)) decls)
       | ABSTYPEdec{abstycs,withtycs,body} => 
	   let fun makeAbstract(GENtyc { eq, ... }) = eq := ABS
		 | makeAbstract _ = bug "makeAbstract"
	       fun redefineEq(DATATYPEdec{datatycs,...}) =
		   let fun setDATA (GENtyc { eq, ... }) = eq := DATA
			 | setDATA _ = ()
		   in
		       app setDATA datatycs;
		       EqTypes.defineEqProps(datatycs,nil,EntityEnv.empty)
		   end
	         | redefineEq(SEQdec decs) = app redefineEq decs
	         | redefineEq(LOCALdec(din,dout)) =
		    (redefineEq din; redefineEq dout)
	         | redefineEq(MARKdec(dec,_)) = redefineEq dec
	         | redefineEq _ = ()
	       val body'= decType0(body,occ,region)
	       val _ = debugmsg ">>decType0: ABSTYPEdec"
	    in app makeAbstract abstycs;
	       redefineEq body';
	       ABSTYPEdec{abstycs=abstycs,withtycs=withtycs,body=body'}
	   end
       | MARKdec(dec,region) => MARKdec(decType0(dec,occ,region),region)

      (* 
       * The next several declarations will never be seen ordinarily;
       * they are for re-typechecking after the instrumentation phase
       * of debugger or profiler. 
       *)
       | STRdec strbs => STRdec(map (strbType(occ,region)) strbs)
       | ABSdec strbs => ABSdec(map (strbType(occ,region)) strbs)
       | FCTdec fctbs => FCTdec(map (fctbType(occ,region)) fctbs)
       | _ => decl

and fctbType (occ,region) (FCTB{fct,def,name}) =
      let fun fctexpType(FCTfct{param, argtycs, def}) =
  	        FCTfct{param=param, def=strexpType (occ,region) def,
	               argtycs=argtycs}
 	    | fctexpType(LETfct(dec,e)) =
	        LETfct(decType0(dec,LetDef occ,region), fctexpType e)
	    | fctexpType(MARKfct(f,region)) = MARKfct(fctexpType f,region)
            | fctexpType v = v
       in FCTB{fct=fct,def=fctexpType def,name=name}
      end

and strexpType (occ,region) (se as (APPstr{oper,arg,argtycs})) = se
  | strexpType (occ,region) (LETstr(dec,e)) =
      LETstr(decType0(dec,LetDef occ,region), strexpType (occ,region) e)
  | strexpType (occ,_) (MARKstr(e,region)) = 
      MARKstr(strexpType (occ,region) e, region)
  | strexpType _ v = v

and strbType (occ,region) (STRB{str,def,name}) =
    STRB{str=str,def=strexpType (occ,region) def,name=name}

val _ = debugmsg ">>decType: resetOverloaded"
val _ = resetOverloaded()
val _ = debugmsg ">>decType: OverloadedLit.reset"
val _ = OLL.reset ()
val _ = debugmsg ">>decType: calling decType0"
val dec' = decType0(dec, if toplev then Root else (LetDef Root), region);
val _ = debugmsg ">>decType: OverloadedLit.resolve"
val _ = OLL.resolve ()
val _ = debugmsg ">>decType: resolveOverloaded"
val _ = resolveOverloaded env
val _ = debugmsg "<<decType: returning"
 in dec'
end (* function decType *)

val decType = Stats.doPhase (Stats.makePhase "Compiler 035 typecheck") decType

end (* local *)
end (* structure Typecheck *)

