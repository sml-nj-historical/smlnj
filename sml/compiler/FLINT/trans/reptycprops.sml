(* reptycprops.sml

   This module preprocesses the absyn for Translate. It fills in tycpath 
   and sigBoundeps information.

   sigBoundeps is a list of the representative entities for a signature
   This is used to determine which tycs in a functor parameter are 
   representative
   (and therefore we need to compute the FLINT kinds)

   Datatype bindings should must be accounted for in Tycpath computations 
   because they will be represented as TP_TYC(...).  
*)

structure RepTycProps =
struct 
	
local
      structure M = Modules
      structure TP = Types
      structure T = TypesTP
      structure EE = EntityEnv
      structure EP = EntPath
      structure SE = StaticEnv
      structure PP = PrettyPrintNew
      structure DI = DebIndex
      structure LT = LtyExtern
      structure TU = TypesUtil
      structure S = Symbol
      structure AT = AbsynTP
      structure V = VarCon

	(* A map from entity TYC or FCT stamps to the first corresponding EP  *)
      structure EPMap = RedBlackMapFn (type ord_key = Stamps.stamp
				       val compare = Stamps.compare)
      (* A StampSet ADT to track of unique stamps (embedded in different
	 structures) we have seen *)
      structure StampSet = RedBlackSetFn (type ord_key = Stamps.stamp
					  val compare = Stamps.compare)
      open Absyn
in
      val debugging = ref false
      val printStrFct = ref true

      (* Support functions *)
      fun debugmsg(m) = if !debugging then print ("RepTycProps: "^m^"\n")
			else ()
      fun bug msg = ErrorMsg.impossible("RepTycProps: " ^ msg)

      local
	  structure ED = ElabDebug
	  fun with_pp f = PP.with_default_pp f
      in 
      fun ppTP tp = print "<tycpath>"
	(* ED.withInternals( 
		fn() => with_pp (
		fn ppstrm => (print "\n"; 
			      PPType.ppTycPath SE.empty ppstrm tp; 
			      print "\n"))) *)
      local 
	  structure PM = PPModules 
      in
        (* s denotes ppstrm *) 

        fun ppSig sign =
	    let val pp = fn s => 
			 (PM.ppSignature s (sign, SE.empty, 20); 
			  print "\n")
	    in
		ED.withInternals (fn() => with_pp pp)
	    end


      fun ppEnt ent =
	with_pp (fn s => (PM.ppEntity s (ent, SE.empty, 20); print "\n"))
      fun ppEntities entenv = 
	with_pp (fn s => (PM.ppEntityEnv s (entenv, SE.empty, 20); print "\n"))
      fun ppTycon tyc =
	ED.withInternals 
	    (fn () => with_pp (fn s => (print "\n"; 
					PPType.ppTycon SE.empty s tyc; 
					print "\n")))
      fun ppEP ep = print "<entpath>"
	(* with_pp (fn s => (print "\n"; 
			     PPModules.ppEntPath s ep; 
			     print "\n")) *)
      fun ppFunsig fctsig = 
	ED.withInternals 
	    (fn () => with_pp (fn s => (PM.ppFunsig s (fctsig, SE.empty, 20); 
					print "\n")))
      end

      end (* local open PPModules *)

      (* fun pk_eqv(k, k') =
	  (case (k, k') 
	    of (AT.PK_MONO, AT.PK_MONO) => true
	     | (AT.PK_SEQ ks, AT.PK_SEQ ks') => ListPair.allEq pk_eqv (ks, ks')
	     | (AT.PK_FUN (pks, bk), AT.PK_FUN (pks', bk')) =>
	       (ListPair.allEq pk_eqv (pks, pks')) andalso (pk_eqv(bk,bk'))
	     | _ => false) *)

      fun eqTycon(T.NoTP tc, T.NoTP tc') = TU.equalTycon(tc,tc')
	| eqTycon _ = raise Fail "Unimplemented"

      fun eqTycPath(T.TP_VAR x, T.TP_VAR x') = 
	  (case (x, x')
	    of (v1 as {tdepth, num, kind}, 
		v2 as {tdepth=tdepth', num=num', kind=kind'}) =>
	       if DI.eq(tdepth,tdepth') andalso 
		  num = num' andalso LT.tk_eqv(kind, kind') (* pk_eqv(kind, kind')*)
		  (* LT.tk_eqv(kind, kind') *) 
	       then true
	       else let fun printTPVar({tdepth, num, kind}) = 
			    (print ("\nTP_VAR("^DI.di_print tdepth^ 
				    ", "^Int.toString num^", "); 
			     (* PP.with_default_pp 
				 (fn ppstrm => PPLty.ppTKind 20 ppstrm kind); *)
				 print ")")
		    in if !debugging 
		       then 
			   (print "\n===eqTycPath TP_VAR unequal===\n";
			    printTPVar v1; print "\n"; printTPVar v2)
		       else (); 
	   false
		    end)
        | eqTycPath(T.TP_TYC tyc, T.TP_TYC tyc') = 
            (debugmsg "--eqTycPath Tycon"; eqTycon(tyc, tyc')) 
	    (* typeutils eqTycon only compares DEFtyc stamps. equalTycon 
	       resolves DEFtycs. Unfortunately, it appears that the tycs
	       this module obtains are reduced forms of the ones 
	       Instantiate produces. 
	     *)
        | eqTycPath(T.TP_FCT(partps, bodytps), T.TP_FCT(partps',bodytps')) =
		ListPair.allEq eqTycPath (partps, partps') andalso
		ListPair.allEq eqTycPath (bodytps, bodytps')
	| eqTycPath(T.TP_APP(tp, argtps), T.TP_APP(tp',argtps')) =
		eqTycPath(tp,tp') 
		andalso ListPair.allEq eqTycPath (argtps, argtps')
  	| eqTycPath(T.TP_SEL(tp, i), T.TP_SEL(tp', i')) = 
	 	eqTycPath(tp,tp') andalso i = i'
	| eqTycPath _ = (debugmsg "--eqTycPath other"; false)
		
    fun checkTycPath(tp, tp') =
	if eqTycPath (tp, tp')
	then true
	else (print "\n===TycPath unequal===\nPrecomputed:\n";
	      ppTP tp; print "\nComputed on-the-fly:\n";
	      ppTP tp'; print "\n\n";
	      false)
	
    fun checkTycPaths(tps, tps') =
	if length tps = length tps' andalso ListPair.allEq eqTycPath (tps, tps')
	then true
	else (print "\n===TycPaths unequal===\nOld:";
	      List.app ppTP tps; print "\nNew:";
	      List.app ppTP tps'; print "\n\n";
	      false)

    (* Processing *)	
    (* entpaths gets all the entspaths from a list of elements 
       We are getting the entpaths from the signature (potentially
       a functor parameter signature). The signature is generally
       untrustworthy because it does not account for external constraints
       such as structure definition (specs). 
	
       functor F(X: sig structure M : sig type t end = A end) = ... 
	  
       The only thing we are interested in is the order of elements 
       given in the signature. 
	  
       Whether entpath should actually processed into a tycpath
       (and not a DATATYPE or DEFtyc) will be determined when we search
       the entity environment. *)
    fun entpaths([]) = []
      | entpaths((_,spec)::rest) =
	(case spec 
	  of M.TYCspec{entVar,info} =>
	     [entVar]::entpaths(rest)
	   | M.STRspec{entVar, sign=M.SIG{elements,...}, def, ...} => 
	     (map (fn ep => entVar::ep) (entpaths elements))@
	     entpaths(rest)
	   | M.FCTspec{entVar, sign, slot} => 
	     [entVar]::entpaths(rest)
	   | _ => entpaths(rest))
    (* fsigInElems : element list -> fctSig list 
       functor signatures in parameter signature order *)
    fun fsigInElems([]) = []
      | fsigInElems((_, spec)::rest) =
	(case spec
	  of M.FCTspec{entVar, sign, slot} => sign::fsigInElems(rest)
	   | M.STRspec{sign=M.SIG{elements=elems,...},...} =>
	       fsigInElems(elems) @ (fsigInElems rest)
	   | _ => fsigInElems rest)
	
    (* repEPs : ep list * EntityEnv -> ep list 
       return the first EPs for each representative TYCent or FCTent 
       only for FORMAL and FLEXTYC though 

       Instantiate should have eliminated any seemingly FORMAL tycs 
       (that were actually constrained by a where or structure definition
        spec) by turning them into DEFtycs. 
     
       The key here is that we need to avoid including duplicate stamps 
       which can be found at the tail of each entpath. *)
    fun repEPs(eps, env) =
        let fun loop([], env, renv, stmpseen) = []
	      | loop(ep::rest, env, renv, stmpseen) =
		let fun proc s = 
			(debugmsg ("--repEPs adding stamp "^
				   Stamps.toShortString s^" path "^
				   EP.entPathToString ep);
			 (case rev ep 
			   of [] => bug "repEPs: empty entpath"
			    | s'::_ =>
			      (case (EPMap.find(renv, s),
				     StampSet.member(stmpseen,s')) 
				of (_, false) => 
				   ep::loop(rest, env, 
					    EPMap.insert(renv,s,ep),
					    StampSet.add(stmpseen,s'))
			  | _ => loop(rest, env, renv, stmpseen))))
		in
		    case EntityEnv.lookEP(env, ep) 
			 handle EntityEnv.Unbound => 
			       (print ("\nrepEPs Unbound "^
				       EP.entPathToString ep^"\n"); 
				raise EntityEnv.Unbound)
		     of M.FCTent{stamp=s,...} => proc s
		      | M.TYCent tyc =>
			   (* If the rlzn says the entity contains a 
			      DATATYPE or a DEFtyc, then we ignore. 
			      Otherwise, we keep it as representative
			      (presumably a FORMAL or FLEXTYC) *)
			(case tyc 
			   of TP.GENtyc{stamp=s,kind,...} =>
				(case kind 
				   of TP.DATATYPE _ => 
				        loop(rest, env, renv, stmpseen)
				    | _ => proc s)
			    | TP.DEFtyc _ => loop(rest,env, renv, stmpseen)
			    | _ => bug "repEPs 0")
		      | _ => bug "repEPs 1"
		   handle EE.Unbound => bug ("repEPs Unbound"^
					     EP.entPathToString ep)
		end
	in loop(eps, env, EPMap.empty, StampSet.empty)	
	end (* fun repEPs *)

    local
	(* Should use tkc_int and tkc_fun instead of these 
	   when TP information is eliminated from Elaborator *)
	      (* fun listofmono(n) = 
		  LT.tkc_int n
	      fun buildKind(n) = 
		  if n <= 0 then LT.tkc_mono
		  else LT.tkc_fun(listofmono n, LT.tkc_mono) *)
	val buildKind = LT.tkc_int
    in
      (* kinds : entenv * ep list * fctsig list -> kind list 
	 Computes the functor kind based on that functor's
	 functor signature and the current entity env. *) 
        fun kinds(eenv, 
		  M.FSIG{paramsig=M.SIG{elements=pelems,...},
			 bodysig=M.SIG{elements=belems,...},...}) = 
	    let val _ = debugmsg ">>kinds\n";
	        val _ = debugmsg "--kinds[FCTent]\n"
		val _ = if !debugging 
			then (print "--kinds eenv\n";
			      ppEntities eenv;
			      print "\n===\n")
			else ()
		val peps = repEPs(entpaths pelems, eenv)
		val _ = debugmsg "--kinds peps computed\n"
		val pfsigs = fsigInElems pelems
		val _ = debugmsg "--kinds pfsigs computed\n"
		(* What is the correct eenv to look up belem entpaths?
		 *)
		(* val beps = repEPs(entpaths belems, eenv)
		   val _ = print "--kinds beps computed\n" *)
		(* val pks = kinds(eenv, peps, pfsigs) *)
		fun loop ([], _) = []
		  | loop (ep::eps, pfsigs) = 
		    (case EE.lookEP(eenv, ep)
			  handle EE.Unbound => 
				 bug ("kinds Unbound "^
				      EP.entPathToString ep)
		      of M.TYCent(TP.GENtyc{kind=TP.DATATYPE _, ...}) =>
			 loop(eps, pfsigs)
		       | M.TYCent(TP.GENtyc{kind, arity, ...}) =>
			 (* Use this when PK eliminated from front-end:
	                    (LT.tkc_int arity)::loop(eps, pfsigs) *)
			 (buildKind arity)::loop(eps, pfsigs)
		       | M.FCTent{paramEnts, 
				  closure=M.CLOSURE{env, ...}, 
				  ...} =>
			 (case pfsigs 
			   of [] => bug "kinds.1"
			    | pfsig::rest => 
			      kinds(eenv, pfsig)::loop(eps, rest))
		       | _ => bug "kinds.0")
	    in (* Use this when PK eliminated from front-end:
	          LT.tkc_fun(loop(peps,pfsigs), LT.tkc_seq []) *)
		LT.tkc_fun(loop(peps,pfsigs), LT.tkc_seq [])
	    end 
	  | kinds _ = bug "kinds.2" (* fun kinds *)
		    
        (* This is the important computation for generating TC_VAR 
	   variable references to functor parameters. 
	   *)  
	fun getTPsforEPs(entenv, eps, fsigs) =
	    let 
		val _ = debugmsg ("--getTPsforEPs eps "^
				  Int.toString (length eps))
			      
		fun loop(entenv, [], i, _) = []
		  | loop(entenv, ep::rest, i, fs) =
		    (debugmsg ("-getTPsforEPs loop "^Int.toString i); 
		     case EE.lookEP(entenv, ep)
			  handle EntityEnv.Unbound =>
				 (print "\ngetTPforEPs for Unbound\n";
				  raise EntityEnv.Unbound)
		      of M.TYCent(tyc as TP.GENtyc{kind=TP.DATATYPE _, ...}) =>
			   T.TP_TYC(T.NoTP tyc)::loop(entenv, rest, i+1, fs)
			   (* Datatypes should be represented directly in the 
			      tycpath *)
		       | M.TYCent(TP.GENtyc{kind=TP.ABSTRACT(tyc),...}) =>
			   (case tyc 
			     of TP.GENtyc{kind=TP.DATATYPE _,...} =>
				T.TP_TYC(T.NoTP tyc)::loop(entenv,rest,i+1,fs)
			      | TP.GENtyc{kind=TP.FORMAL, arity, ...} => 
				(T.TP_VAR{tdepth=DI.top,num=i,
					 kind=buildKind arity})::
				loop(entenv, rest, i+1, fs)
			      | _ => 
				T.TP_TYC(T.NoTP tyc)::loop(entenv,rest,i+1,fs))
		      
		       | M.TYCent(TP.GENtyc{kind, arity, ...}) =>
			 let val _ = debugmsg "--getTPsforEPs[TYCent GENtyc]"
			     val kind = (* LT.tkc_int(arity) *)
				 buildKind arity
			     val var = {tdepth=DI.top,num=i,
					kind=kind}
			     val tp' = T.TP_VAR var
			       (* val _ = checkTycPath(tp, tp') *)
			   in tp'::loop(entenv, rest, i+1, fs)
			   end
		       | M.TYCent tyc => 
			    (debugmsg "--getTPsforEPs[TYCent]";
			     T.TP_TYC(T.NoTP tyc)::loop(entenv, rest, i+1, fs))
			    (* [TODO] What to do about GENtyc FORMAL? *) 
		       | M.FCTent {(* tycpath=SOME tp,*) paramEnts, ...} => 
			   (debugmsg "--getTPsforEPs[FCTent SOME]";
			    (case fs
			      of [] => bug "getTPsforEPs.1"
			       | f::srest => 
				 let val _ = 
					 if !debugging then 
					     (print "\n===FCTent paramEnts===\n";
					      ppEntities paramEnts;
					      print "\n===FCTent eenv===\n";
					      ppEntities entenv;
					      print "\n--kinds[FCTent] Funsig\n";
					      ppFunsig f; print "\n")
					 else ()
						
				     val kind = kinds(paramEnts, f)   
				     val _ = debugmsg "<<kinds done\n"
				     val var = {tdepth=DI.top, num=i,
						kind=kind}
				     val tp' = T.TP_VAR var 
				(* val _ = checkTycPath(tp, tp') *)
				 in tp'::loop(entenv, rest, i, srest)
				 end)) (* FIXME *)
		       | _ => bug "getTPforEPs 0")
		    handle EE.Unbound => bug "getTPforEPs Unbound"
		in loop(entenv, eps, 0, fsigs)
		end (* fun getTPsforEPs *)
	      
	      fun getTk(M.FSIG{paramsig=M.SIG ps, ...}, dummyEnts, argEnts, 
			fsigs) =
		  let 
		      val alleps = 
			  entpaths(#elements ps)
		      val fsigs = fsigInElems(#elements ps)
		      val eps = repEPs(alleps, dummyEnts)
		      val argtycs' = 
			  getTPsforEPs(argEnts, eps, fsigs)
		  in argtycs'
		  end (* getTk *)
		| getTk _ = bug "getTk 0"

      end (* local *)

    fun procCloSE(se) =
	(case se 
	  of M.VARstr _ => se
	   | M.CONSTstr _ => se
	   | M.STRUCTURE _ => se
	   | M.APPLY(fctExp, strExp) =>
	     M.APPLY(procCloFE fctExp, procCloSE strExp) 
	   | M.LETstr(entDec, strExp) =>
	     M.LETstr(entDec, procCloSE strExp)
	   | M.ABSstr(sign, strExp) =>
	     M.ABSstr(sign, procCloSE strExp)
	   | M.CONSTRAINstr{boundvar,raw,coercion} =>
	     M.CONSTRAINstr{boundvar=boundvar,raw=raw,
			    coercion=procCloSE coercion}
	   | M.FORMstr _ => bug "unexpected FORMstr in procCloSE")
    and procCloFE(fe) =
	(case fe
	  of M.VARfct _ => fe
	   | M.CONSTfct _ => fe
	   | M.LAMBDA{body,param,paramEnts} => 
	     M.LAMBDA{param=param, body=procCloSE body, paramEnts=paramEnts}
	   | M.LETfct(entDec, fexp) => M.LETfct(entDec, procCloFE fexp)
	   | M.LAMBDA_TP _ => bug "procCloFE bug LAMBDA_TP")

    (* exception TransVar
    fun transVar V.ERRORvar = raise TransVar
      | transVar(V.OVLDvar{name,options,scheme}) =
	let 
	    fun transopts([]) = []
	      | transopts({indicator,variant}::rest) =
		let val indicator' = T.TyNoTP indicator
		    val opt = {indicator=indicator',
			       variant=transVar variant}
		in opt::transopts(rest)
		end
	in
	    AT.OVLDvar{name=name, options=ref (transopts (!options)),
		       scheme=scheme}
	end
      | transVar(V.VALvar{path,typ,access,prim}) =
	AT.VALvar{path=path,typ=ref (T.TyNoTP (!typ)), access=access,
		  prim=prim}
    (* end transVar *) *)


    (* dec * DebIndex.depth -> 
	     dec with tycpaths and memoized ep * tkind lists 
       This code needs EPMap (don't forgot EPMap in the local ... in 
       bindings section above ...)
     *)
    fun procDec (dec, d : DebIndex.depth) =
	let 
	    val _ = debugmsg ">>procDec"
	    fun procStrexp def =
		(case def 
		  of (APPstr{oper=oper 
			      as M.FCT{sign=fctsign 
					as M.FSIG {paramsig=fsparsig 
							as M.SIG fsr,...},
				       rlzn=fctRlzn,access,prim},
			     arg=arg
				as M.STR{sign=argsig 
				            as M.SIG{elements,...},
			rlzn=argRlzn as {entities,...},...}, ...}) => 
		     let val {paramEnts=dummyEnts, 
			      closure=M.CLOSURE{body,param=fclparam,
						env=fclenv},
			      stamp=fstmp,
			      properties=fprops,
			      rpath=frp,
			      stub=fstub} = 
			     fctRlzn 
			 val _ = debugmsg "--strBinds APPstr"
			 val _ = if !debugging then 
				     (debugmsg "===fsparsig===";
				     ppSig fsparsig;
				     debugmsg "===dummyEnts===";
				     ppEntities dummyEnts;
				     debugmsg "===argsig===";
				     ppSig argsig;
				     debugmsg "===argEnts===";
				     ppEntities entities) 
				 else () 
			 val alleps =
			     entpaths(#elements fsr)
			 val fsigs = fsigInElems(#elements fsr)
			 val eps = repEPs(alleps, dummyEnts)
			 val argtycs' = 
			     getTPsforEPs(entities, eps,
					  fsigs)
			 val body' = procCloSE(body)
			 val fcl' = 
			     M.CLOSURE{param=fclparam,
				       env=fclenv,
				       body=body'}
			 val fctRlzn' = 
			     {paramEnts=dummyEnts,
			      stamp=fstmp,
			      properties=fprops,
			      rpath=frp,
			      stub=fstub,
			      closure=fcl'}
			 val oper' = 
			     M.FCT{sign=fctsign, 
				   access=access,
				   prim=prim,
				   rlzn=fctRlzn'} 
			 (* getTycPaths(fsparsig, dummyEnts) *)
			 val se' = AT.APPstr {oper=oper', arg=arg, 
					      argtycs=argtycs'}
		     (* Check that argtycs = argtycs' here *)
		     (* val _ = if checkTycPaths(argtycs, argtycs') 
				   orelse (not (!printStrFct))
				then ()
				else (print "\nrepEPs:\n";
				      app ppEP eps;
				      print "\nfsparsig:\n";
				      ppSig fsparsig;
				      print "\nargsig:\n";
				      ppSig argsig;
				      print "\nfctRlzn:\n";
				      ppEnt (M.FCTent fctRlzn);
				      print "\nargRlzn:\n";
				      ppEnt (M.STRent argRlzn);
				      print "\ndummyEnts:\n";
				      ppEntities dummyEnts;
				      bug "Wrong arg tycs")  *)
		     (* val _ = if length argtycs <> length argtycs'
				then bug "strBinds: bad argtycs computation"
				else ()	 *)
		     in se'
		     end 
		   | APPstr {oper=M.FCT _, arg=M.STRSIG _, ...} => 
		     bug "strBinds: Unimplemented"
		   | LETstr(dec,se') => 
		     AT.LETstr(procDec(dec,d), procStrexp se')
		   | VARstr s => AT.VARstr s
		   | MARKstr(se',r) => AT.MARKstr(procStrexp se', r)
		   | STRstr binds => AT.STRstr binds
		   | _ => bug "procStrexp")
		    
	    fun fctBinds([]) = []
	      | fctBinds((b as FCTB{fct=fct 
				as M.FCT{sign=M.FSIG{paramsig=paramsig'
			as M.SIG fsr,paramvar,...},
			rlzn={paramEnts,...},
		...}, def, name})::rest) =
		let fun mkFctexp(fe) =
			(case fe 
			  of VARfct f => 
			     AT.VARfct f 
			   | FCTfct{param=param 
				      as M.STR{sign=paramsig 
						as M.SIG sr, 
					       rlzn=rlzn 
						as {entities,
						    ...},...},
				    def} =>
			     (debugmsg (">>fctBinds:mkFctexp[FCTfct] name "^
					S.name name); 
			      let 
				  val alleps = 
				      entpaths (#elements fsr)
				  val _ = if !debugging 
					  then (print "--fctBinds[FCTfct] paramsig'\n"; 
						ppSig paramsig';
						print "\n--fctBinds[FCTfct] paramsig\n";
						ppSig paramsig;
						print "\n--fctBinds[FCTfct] paramEnts\n";
						ppEntities paramEnts;
						print "\n--fctBinds[FCTfct] rlznEntities\n";
						ppEntities entities) 
					  else ()
				  val fsigs = fsigInElems(#elements fsr)  
				  val eps = repEPs(alleps, entities)
				  val argtycs' = getTPsforEPs(entities, eps,fsigs) 
			      (*val _ = printArgtycs(argtycs, argtycs')
				val _ = if length argtycs' <> length argtycs
					then bug "fctBinds argtycs bad computation"
					else ()*)
			      (* val _ = if checkTycPaths(argtycs, argtycs')
					    orelse (not (!printStrFct))
					 then ()
					 else (print "\nrepEPs:\n";
					       app ppEP eps;
					       print "\nparamsig:\n";
					       ppSig paramsig;
					       print "\nentities:\n";
					       ppEntities entities; 
					       print "\nparamsig':\n";
					       ppSig paramsig'; print "\n"
					bug "wrong arg tycs mkFctexp") *)
			      in AT.FCTfct{param=param,def=procStrexp def,
					   argtycs=argtycs'}
			      end)
			   | MARKfct(fe',region) => 
			     AT.MARKfct(mkFctexp fe',region)
			   | LETfct(dec', fe') => 
			     AT.LETfct(procDec(dec',d), mkFctexp fe')
			   | _ => bug "mkFctexp 0")
		in AT.FCTB{name=name, fct=fct, 
			   def=mkFctexp(def)} :: fctBinds(rest)
		end
	      | fctBinds _ = bug "fctBinds: unexpected binding"
		
	    fun strBinds([]) = []
	      | strBinds((b as STRB{name, str, def})::rest) =
		let val _ = debugmsg (">>strBinds "^
				      Symbol.symbolToString name)
			val sb' = AT.STRB{name=name, str=str, 
					  def=procStrexp def}
		in
		    sb' :: strBinds(rest)
		end (* fun strBinds *)
		    
	    fun transVB(VB {pat,exp,boundtvs,tyvars}) = 
		AT.VB{pat=pat,exp=transExp d exp,boundtvs=boundtvs,
		      tyvars=tyvars}
	    fun transRVB(RVB{var,exp,boundtvs,resultty,tyvars}) =
		AT.RVB{var=var,exp=transExp d exp,boundtvs=boundtvs,
		       resultty=resultty,tyvars=tyvars}
	in
	    (case dec 
	      of SEQdec(decs) => 
		 AT.SEQdec(map (fn(dec') => procDec(dec',d)) decs)
	       | LOCALdec(dec1, dec2) => 
		 AT.LOCALdec(procDec(dec1,d), procDec (dec2,d))	
	       | MARKdec(dec',r) => AT.MARKdec(procDec(dec',d), r)
	       | FCTdec(fctbs) => AT.FCTdec(fctBinds fctbs)  
	       | STRdec(strbs) => AT.STRdec(strBinds strbs)
	       | ABSdec strbs => AT.ABSdec(strBinds strbs)
	       | OPENdec x => AT.OPENdec x (* May have module dec *)
	       | SIGdec bs => AT.SIGdec bs
	       | FSIGdec bs => AT.FSIGdec bs
	       | VALdec vbs => AT.VALdec (map transVB vbs)
	       | VALRECdec rvbs => AT.VALRECdec (map transRVB rvbs)
	       | TYPEdec tycs => AT.TYPEdec tycs
	       | DATATYPEdec x => AT.DATATYPEdec x
	       | ABSTYPEdec{abstycs, body, withtycs} => 
		 AT.ABSTYPEdec{abstycs=abstycs, body=procDec(body,d), 
			       withtycs=withtycs}
	       | EXCEPTIONdec ebs => 
		 AT.EXCEPTIONdec (map (fn (EBgen{exn,etype,ident}) => 
					  AT.EBgen{exn=exn,etype=etype,
						   ident=transExp d ident}
					| (EBdef{exn,edef}) => 
					  AT.EBdef{exn=exn,edef=edef})
				      ebs)
	       | OVLDdec v => AT.OVLDdec(v)
	       | FIXdec x => AT.FIXdec x)
	end
    and transExp d e = 
	(let val transExp' = transExp d
	     fun transRule(RULE(p,e)) = AT.RULE(p,transExp' e)
	     fun transFnRules(rules, ty) = (map transRule rules, ty)
	 in
	     (case e 
	       of VARexp(v,tyvars) => AT.VARexp(v,tyvars)
		| (CONexp d) => AT.CONexp d
		| (INTexp d) => AT.INTexp d
		| (WORDexp d) => AT.WORDexp d
		| (REALexp d) => AT.REALexp d
		| (STRINGexp d) => AT.STRINGexp d
		| (CHARexp d) => AT.CHARexp d
		| (RECORDexp recs) => 
		  AT.RECORDexp(map (fn(lab,e) => (lab,transExp' e)) recs)
		| (SELECTexp(lab,e)) => AT.SELECTexp (lab, transExp' e)
		| (VECTORexp(es, ty)) => AT.VECTORexp(map transExp' es, ty)
		| (APPexp(e1,e2)) => AT.APPexp(transExp' e1, transExp' e2)
		| (HANDLEexp(e,rules)) => 
		  AT.HANDLEexp(transExp' e, transFnRules rules)
		| RAISEexp(e,ty) => 
		  AT.RAISEexp(transExp' e, ty)
		| CASEexp(e,rules, m) => 
		  AT.CASEexp(transExp' e, map transRule rules,m)
		| IFexp{test,thenCase,elseCase} =>
		  AT.IFexp{test=transExp' test, 
			   thenCase=transExp' thenCase,
			   elseCase=transExp' elseCase}
		| ANDALSOexp(e1,e2) => AT.ANDALSOexp(transExp' e1, transExp' e2)
		| ORELSEexp(e1,e2) => AT.ORELSEexp(transExp' e1, transExp' e2)
		| WHILEexp{test,expr} => 
		  AT.WHILEexp{test=transExp' test,expr=transExp' expr}
		| FNexp(fnrules) => AT.FNexp(transFnRules fnrules)
		| LETexp(dec,e) => AT.LETexp(procDec (dec,d), transExp' e)
		| SEQexp(es) => AT.SEQexp(map transExp' es)
		| CONSTRAINTexp(e,t) => AT.CONSTRAINTexp(transExp' e, t)
		| MARKexp(e,r) => AT.MARKexp(transExp' e, r))
	 end) (* transExp *)				  

end (* local *)
	
end (* structure RepTycProps *)
