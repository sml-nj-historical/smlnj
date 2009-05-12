(* reptycprops.sml

   This module processes the static information (realizations and signatures)
   to obtain primary type component information for Translate. It fills in 
   tycpath and sigBoundeps information.

   sigBoundeps is a list of the representative entities for a signature
   This is used to determine which tycs in a functor parameter are 
   representative
   (and therefore we need to compute the FLINT kinds)

   Datatype bindings must be accounted for in Tycpath computations 
   because they will be represented as TP_TYC(...).  
*)

(* DBM [4/7/09]
 * all references to bodyRlzn have been removed, and functor closures should
 * be used instead.
 *)

signature REPTYCPROPS = 
sig
   val getTk : Modules.fctSig * Modules.strEntity * DebIndex.depth 
	       -> (TycPath.tycpath list * TycPath.tycpath FlexTycMap.map)  
   val primaryCompInStruct : TycPath.tycpath FlexTycMap.map * Modules.strEntity
			     * Modules.strEntity option * Modules.Signature 
			     * DebIndex.depth 
			     -> TycPath.tycpath FlexTycMap.map 
				* TycPath.tycpath list
end

structure RepTycProps : REPTYCPROPS =
struct 
	
local
      structure M = Modules
      structure T = Types
      structure TP = TycPath
      structure EE = EntityEnv
      structure EP = EntPath
      structure SE = StaticEnv
      structure PP = PrettyPrintNew
      structure DI = DebIndex
      structure LT = LtyExtern
      structure TU = TypesUtil
      structure S = Symbol
      structure V = VarCon
      structure FTM = FlexTycMap

      (* A StampSet ADT to keep track of unique stamps (embedded in different
	 structures) we have seen *)
      structure StampSet = RedBlackSetFn (type ord_key = Stamps.stamp
					  val compare = Stamps.compare)

      open Absyn

      fun unionMaps []  = FTM.empty
	| unionMaps (m::ms) = FTM.unionWith(fn(tp1,tp2) => tp1)
					   (m, unionMaps ms)

in
      val debugging = FLINT_Control.trdebugging
      val printStrFct = ref true

      (* Support functions *)
      fun debugmsg(m) = 
          if !debugging then print ("RepTycProps: "^m^"\n") else ()

      fun bug msg = ErrorMsg.impossible("RepTycProps: " ^ msg)

      fun insertMap(m, x, obj) = 
	  (debugmsg ("--insertMap "^Stamps.toShortString x); 
	   (case FTM.find(m, x) 
	     of SOME _ => m
	      | NONE => (FTM.insert(m, x, obj))))

      (* prettyprinting functions for debugging *)
      local
	  structure ED = ElabDebug
	  structure PM = PPModules 
	  fun with_pp f = PP.with_default_pp f
      in 
        fun ppTP tp = print "<tycpath>"
	(* ED.withInternals( 
		fn() => with_pp (
		fn ppstrm => (print "\n"; 
			      PPType.ppTycPath SE.empty ppstrm tp; 
			      print "\n"))) *)

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

      end (* local ElabDebug, PPModules *)

    (* Processing *)	
    (* entpaths gets all the entpaths from a list of elements 
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
	   | M.FCTspec{entVar, sign, slot} => 
	     [entVar]::entpaths(rest)
	   | M.STRspec{entVar, sign=M.SIG{elements,...}, def, ...} => 
	     (map (fn ep => entVar::ep) (entpaths elements)) @
	     entpaths(rest)
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
       only for FORMAL though.

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
			      (debugmsg ("--repEPs add stamp "^
					 Stamps.toShortString s'^
					 " to stmpseen");
			       (case (FTM.find(renv, s),
				     StampSet.member(stmpseen,s')) 
				of ((_, false) | (NONE, _)) => 
				   ep::loop(rest, env, 
					    FTM.insert(renv,s,ep),
					    StampSet.add(stmpseen,s'))
				 | _ => loop(rest, env, renv, stmpseen)))))
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
			      (presumably a FORMAL) *)
			(case tyc 
			   of T.GENtyc{stamp=s,kind,...} =>
				(case kind 
				   of T.DATATYPE _ => 
				        loop(rest, env, renv, stmpseen)
				    | _ => proc s)
			    | T.DEFtyc _ => loop(rest,env, renv, stmpseen)
			    | _ => bug "repEPs 0")
		      | M.STRent _ => bug "repEPs 1"
		      | M.ERRORent => (* in MLRISC/ra/risc-ra.sml this actually happens *)
			loop(rest,env,renv, stmpseen)
		   handle EE.Unbound => bug ("repEPs Unbound"^
					     EP.entPathToString ep)
		end
	in loop(eps, env, FTM.empty, StampSet.empty)	
	end (* fun repEPs *)

    local

	(* Should use tkc_int and tkc_fun instead of these 
	   when TP information is eliminated from Elaborator *)
	val buildKind = LT.tkc_int
    in
      (* kinds : entenv * entenv * fctsig -> kind
	 Computes the functor kind based on that functor's
	 functor signature and the current entity env. *) 
      fun kinds(paramEnts, bodyEnts, 
		M.FSIG{paramsig=M.SIG{elements=pelems,...},
		       bodysig=M.SIG{elements=belems,...},...}) = 
	    let val _ = debugmsg ">>kinds\n";
	        val _ = debugmsg "--kinds[FCTent]\n"
		val _ = if !debugging 
			then (print "--kinds eenv\n";
			      ppEntities paramEnts;
			      print "\n===\n")
			else ()
		val peps = repEPs(entpaths pelems, paramEnts)
		val _ = debugmsg "--kinds peps computed"
		val pfsigs = fsigInElems pelems
		val _ = debugmsg "--kinds pfsigs computed"

	        (* [TODO] This can be a problem. belems can refer to 
		   formal functor body for curried functor,
		   but formal body signature has not been
		   instantiated with the actual argument realization. *)
		val beps = repEPs(entpaths belems, bodyEnts)
		val _ = debugmsg "--kinds beps computed\n"
		val bfsigs = fsigInElems belems
		val _ = debugmsg "--kinds bfsigs computed\n"
		(* What is the correct eenv to look up belem entpaths?
		 *)
		fun loop ([], _, eenv) = []
		  | loop (ep::eps, fsigs, eenv) = 
		    (case EE.lookEP(eenv, ep)
			  handle EE.Unbound => 
				 bug ("kinds Unbound "^
				      EP.entPathToString ep)
		      of M.TYCent(T.GENtyc{kind=T.DATATYPE _, ...}) =>
			 loop(eps, fsigs, eenv)
		       | M.TYCent(T.GENtyc{kind, arity, ...}) =>
			 (* Use this when PK eliminated from front-end:
	                    (LT.tkc_int arity)::loop(eps, pfsigs) *)
			 (buildKind arity)::loop(eps, fsigs, eenv)
		       | M.FCTent{paramRlzn, closure=M.CLOSURE{env, ...}, ...} =>
                         (* This should be using closure and paramRlzn --
			  * bodyRlzn has been deleted *)
			 (case fsigs 
			   of [] => bug "kinds.1"
			    | fsig::rest => 
			      kinds(#entities paramRlzn, 
				    (raise Fail "#entities bodyRlzn"), fsig)::
			      loop(eps, rest, eenv))
		       | _ => bug "kinds.0")

		val paramtk = loop(peps,pfsigs,paramEnts)
		val _ = debugmsg "--kinds paramtk computed"
		val bodytk = loop(beps,bfsigs, bodyEnts)
		val _ = debugmsg "--kinds bodytk computed"
	    in (* Use this when PK eliminated from front-end: *)
		LT.tkc_fun(paramtk, LT.tkc_seq bodytk)
	    end 
	  | kinds _ = bug "kinds.2" (* fun kinds *)		  

	fun formalBody(ftmap0, bodyEnts, argTps, msig as M.SIG{elements, ...}, 
		       paramEnts, fsig, d, i) =
	    let val _ = debugmsg "--in formalBody kinds"

		val M.FSIG{paramsig=M.SIG{elements=pelems,...},
			   bodysig=M.SIG{elements=belems,...},...} = fsig
		val peps = repEPs(entpaths pelems, paramEnts)
		val _ = debugmsg "--formalBody peps computed"
		val pfsigs = fsigInElems pelems
		val _ = debugmsg "--formalBody pfsigs computed"

		fun loopkind ([], _, eenv) = []
		  | loopkind (ep::eps, fsigs, eenv) = 
		    (case EE.lookEP(eenv, ep)
			  handle EE.Unbound => 
				 bug ("kinds Unbound "^
				      EP.entPathToString ep)
		      of M.TYCent(T.GENtyc{kind=T.DATATYPE _, ...}) =>
			 loopkind(eps, fsigs, eenv)
		       | M.TYCent(T.GENtyc{kind, arity, ...}) =>
			 (* Use this when PK eliminated from front-end:
	                    (LT.tkc_int arity)::loop(eps, pfsigs) *)
			 (buildKind arity)::loopkind(eps, fsigs, eenv)
		       | M.FCTent{paramRlzn, closure=M.CLOSURE{env, ...}, ...} =>
                         (* this should be using closure and paramRlzn *)
			 (case fsigs 
			   of [] => bug "kinds.1"
			    | fsig::rest => 
			      kinds(#entities paramRlzn, 
				    (raise Fail "#entities bodyRlzn"), fsig)::
			      loopkind(eps, rest, eenv))
		       | _ => bug "kinds.0")

		val paramtk = loopkind(peps,pfsigs,paramEnts)
		val _ = debugmsg "--formalBody paramtk computed"

	        (* [TODO] This can be a problem. belems can refer to 
		   formal functor body for curried functor,
		   but formal body signature has not been
		   instantiated with the actual argument realization. *)
		val beps = repEPs(entpaths belems, bodyEnts)
		val _ = debugmsg "--formalBody beps computed\n"
		val bfsigs = fsigInElems belems
		val _ = debugmsg "--formalBody bfsigs computed\n"
		(* What is the correct eenv to look up belem entpaths?
		 *)

		val bodytk = loopkind(beps,bfsigs, bodyEnts)
		val _ = debugmsg "--formalBody bodytk computed"

		val kind = LT.tkc_fun(paramtk, LT.tkc_seq bodytk)
		val _ = (debugmsg ("--formalBody elements ");
			 if !debugging then ppSig msig else ())
	
		val eps = entpaths(elements)
		val _ = debugmsg ("--formalBody eps "^Int.toString (length eps))
		fun loop(ftmap, eenv, [], j, tps) = (ftmap, rev tps)
		  | loop(ftmap, eenv, ep::rest, j, tps) = 
		    (case EE.lookEP(eenv, ep)
		      of M.TYCent(T.GENtyc{kind=T.DATATYPE _, stamp, ...}) =>
			 let val _ = debugmsg ("--formalBody DATATYPE "^
					       Stamps.toShortString stamp)
			 in loop(ftmap, eenv, rest, j, tps) 
			 end
		       | M.TYCent(T.GENtyc{stamp, ...}) =>
			 let val fctvar = TP.TP_VAR{tdepth=d, num=i, kind=kind}
			     val tp = TP.TP_SEL(TP.TP_APP(fctvar, argTps), j)
			     val _ = debugmsg ("--formalBody "^
					       Stamps.toShortString stamp^
					       " is index "^
					       Int.toString j)
			 in case FTM.find(ftmap, stamp)
			     of SOME _ => loop(ftmap, eenv, rest, j, tps)
			      | NONE => loop(insertMap(ftmap, stamp, tp),
					     eenv,
				 rest, j+1, tp::tps)
			 end
		       | M.TYCent _ => 
			 (debugmsg "--formalBody other TYCent GEN";
			  loop(ftmap, eenv, rest, j, tps))
		       | M.FCTent _ =>
			 (debugmsg "--formalBody FCTent";
			  loop(ftmap, eenv, rest, j, tps))
		       | _ => (debugmsg "--formalBody other ent";
			       loop(ftmap, eenv, rest, j, tps)))
		    val (ftmap1, tps) = loop(ftmap0, bodyEnts, beps, 0, [])
	    in (ftmap1, tps)
	    end
	  | formalBody _ = bug "Unexpected signature in formalBody"

        (* There are two kinds of tycpath computations, one for 
         * functor parameters and the other for functor parameter
         * references in the body of a functor. In either case, 
         * we want to use the deBruijn index depth at the 
         * site of definition and not the incidental depth at the 
         * site of occurrence. 
         *)  
       (* This is the important computation for generating TC_VAR 
	   variable references to functor parameters. 

	  FTM.map * M.strEntity * M.strEntity * M.sigrec * DI.depth
	  -> FTM.map * tycpath list 
	   *)  
	(* The goal here, simply put, is to get the primary components
	   in rlzn where a component is primary if it is a representative
	   picked by instantiate in freerlzn. *)
	fun primaryCompInStruct(ftmap0, freerlzn : M.strEntity, 
				rlznOp: M.strEntity option, 
				M.SIG (sign : M.sigrec), 
				d) =
	    let
		val fsigs = fsigInElems(#elements sign)
		val _  = debugmsg ("--pri num of fsigs "^
				   Int.toString (length fsigs)
				   ^" depth "^DI.dp_print d)
		val rlzn = (case rlznOp of NONE => freerlzn | SOME r => r)
		val entenv = #entities rlzn
		val eps = repEPs(entpaths(#elements sign), #entities freerlzn) 		
		val _ = debugmsg ("--primaryCompInStruct eps "^
				  Int.toString (length eps)^
				  " d="^DI.dp_print d)
		fun loop(ftmap, tps, entenv, [], i, _) = (ftmap, rev tps)
		  | loop(ftmap, tps, entenv, ep::rest, i, fs) =
		    (debugmsg ("-primaryCompInStruct loop "^Int.toString i); 
		     let val ev : Stamps.stamp = hd (rev ep)
		     in 
		     case EE.lookEP(entenv, ep)
			  handle EntityEnv.Unbound =>
				 (print "\npri for Unbound\n";
				  raise EntityEnv.Unbound)
		      of M.TYCent(tyc as T.GENtyc{kind=T.DATATYPE _, stamp,...}) =>
			   let val tp = TP.TP_TYC(tyc)
			   in (loop(insertMap(ftmap, stamp, tp), 
				    tp::tps, entenv, rest, i+1, fs))
			   end
			   (* Datatypes should be represented directly in the 
			      tycpath *)
		       | M.TYCent(T.GENtyc{kind=T.ABSTRACT(tyc),stamp=s1,...}) =>
			   let val (tp,s) = 
				    (case tyc 
			     of T.GENtyc{kind=T.DATATYPE _,stamp,...} =>
				(TP.TP_TYC(tyc), stamp)
			      | T.GENtyc{kind=T.FORMAL, arity, stamp, ...} => 
				(case FTM.find(ftmap, stamp)
				  of SOME tp' => (tp', stamp)
				   | NONE => 
				     (debugmsg ("--eps VAR depth "^DI.dp_print d);
				      (TP.TP_VAR{tdepth=d,num=i,
					 kind=buildKind arity}, stamp)))
			      | _ => 
				(debugmsg "--pri[GEN] nonformal/data abstract";
				 (TP.TP_TYC( tyc), s1)))
			    in 
			       loop(insertMap(ftmap, s, tp), 
				    tp::tps, entenv,rest,i+1,fs)
			    end
		      
		       | M.TYCent(T.GENtyc{kind, arity, stamp, ...}) =>
			 let val _ = debugmsg "--primaryCompInStruct[TYCent GENtyc]"
			     val kind = buildKind arity
	                     (* Check if stamp is previously defined. 
			      * If so, then this must be a variable occurrence
			      * and not a functor parameter binding
			      * so use the depth at the definition site 
			      * (i.e., in the ftmap0 tycpath) instead of the 
			      * current occurrence site depth. *)
			     val tp' = 
				 (case FTM.find(ftmap, stamp)
				   of SOME tp' => 
				      (debugmsg ("--primaryCompInStruct[TYCent GENtyc] found stmp "^Stamps.toShortString stamp); 
				       tp')
				    | NONE => 
				      (debugmsg ("--primaryCompInStruct[TYCent GENtyc] generating "^Stamps.toShortString stamp^" depth="^DI.dp_print d); 
				       TP.TP_VAR {tdepth=d,num=i, kind=kind}))
			       (* val _ = checkTycPath(tp, tp') *)
			   in 
			     loop(insertMap(ftmap, stamp, tp'),
				  tp'::tps, entenv, rest, i+1, fs)
			   end
		       | M.TYCent tyc => 
			    (debugmsg "--primaryCompInStruct[TYCent]";
			     (let val tp = TP.TP_TYC(tyc)
			      in loop(insertMap(ftmap, ev, tp),
				      tp::tps, entenv, rest, i+1, fs)
		              end))
		       | M.FCTent {stamp, paramRlzn,
				   closure=M.CLOSURE{env=closenv,...},...} => 
			   (debugmsg "--primaryCompInStruct[FCTent SOME]";
                           (* this should be using closure and paramRlzn *)
			    ( case fs
			      of [] => bug "primaryCompInStruct.1"
			       | (fsig as M.FSIG{bodysig=bsig as M.SIG bsr,
					      paramsig=paramsig as M.SIG psr, 
					      ...})::srest => 
				 let 
				     (* If FCTent is a result from a partially
				        applied functor, then the given bsr
					is no longer reliable, because it 
					may only give the signature of the 
					result after curried application when
					we need the signature for the 
					original functor before partial 
					application *)
				     val paramEnts = #entities paramRlzn
				     val bodyEnts = raise Fail "#entities bodyRlzn"
				     val _ = 
					 if !debugging then 
					     (print "\n===FCTent paramEnts===\n";
					      ppEntities paramEnts;
					      print "\n===FCTent bodyEnts===\n";
					      ppEntities bodyEnts;
					      print "\n===FCTent closenv===\n";
					      ppEntities closenv;
					      print "\n--kinds[FCTent] Funsig\n"
					      )
					 else ()

				     val argRepEPs = 
					 repEPs(entpaths(#elements psr),
						#entities paramRlzn)
				     val (ftmap1, argtps) = 
					 primaryCompInStruct(ftmap,
							     paramRlzn,
							     NONE,
							     paramsig,
							     DI.next d)

				    
                                     (* [TODO] Replace free instantiation
				        components with actual argument 
					realization components. *)

				     (* Can't do normal flextyc tycpath 
				        construction here because actual 
					argument is not yet available.
					Must traverse bodyRlzn in signature
					order and add TP_SEL(TC_APP(tv,args),i)
					for each FORMAL GENtyc *)
				     val _ = debugmsg ("pri[FCT]TP_VAR depth "^
						       DI.dp_print d)
			       (* BOGUS! bodyRlzn is being DELETED *)
				     val (ftmap2,bodytps) = 
					 formalBody(ftmap1, (raise Fail "#entities bodyRlzn"),
						    argtps, bsig, 
						    paramEnts,
						    fsig, d, i)
				     val tp' = TP.TP_FCT(argtps, bodytps)
				 in 
				    loop(ftmap2,
					 tp'::tps, entenv, rest, i+1, srest)
				 end
			        | _ => bug "unexpected errorFSIG")) 
		       | _ => bug "primaryCompInStruct 0"
		     end (* loop *) )
		    handle EE.Unbound => bug "primaryCompInStruct Unbound"
		in loop(ftmap0, [], entenv, eps, 0, fsigs)
	    end (* fun primaryCompInStruct *)
	  | primaryCompInStruct _ = bug "Unexpected error signature"

        (* Get the primary components in a realization R0 but replacing any 
	   occurrences of entities in a realization R1 with the 
	   corresponding entities in a realization R2. *) 
	(* fun primariesWithParamRepl(ftmap0, bodyRlzn : M.strEntity,
				   freeRlzn : M.strEntity, 
				   argRlzn : M.strEntity,
				   sign : M.sigrec, d) =
	    let *)
		

	fun getTk(M.FSIG{paramsig=paramsig as M.SIG ps, ...}, argRlzn, 
		  d) =
	    let 
		val _ = debugmsg ">>getTk"
		val (ftmap, argtycs') = 
		    primaryCompInStruct(FTM.empty, argRlzn, NONE, 
					paramsig, d)
		val _ = debugmsg "<<getTk"
	    in (argtycs', ftmap)
	    end (* getTk *)
	  | getTk _ = bug "getTk 0"

    end (* local *)
end (* local *)
	
end (* structure RepTycProps *)
