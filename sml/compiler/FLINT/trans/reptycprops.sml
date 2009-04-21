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
 * all references to bodyRlzn need to be removed, and functor closures should
 * be used instead.
 *)

signature REPTYCPROPS = 
sig
   val getTk : Modules.fctSig * Modules.strEntity * Modules.strEntity 
	       * DebIndex.depth 
	       -> (TypesTP.tycpath list * TypesTP.tycpath FlexTycMap.map)  
   val primaryCompInStruct : TypesTP.tycpath FlexTycMap.map * Modules.strEntity
			     * Modules.strEntity * Modules.Signature 
			     * DebIndex.depth 
			     -> TypesTP.tycpath FlexTycMap.map 
				* TypesTP.tycpath list
end

structure RepTycProps : REPTYCPROPS =
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
(*      structure AT = AbsynTP *)
      structure V = VarCon
      structure FTM = FlexTycMap

      (* A map from entity TYC or FCT stamps to the first corresponding EP *)
      structure EPMap = RedBlackMapFn (type ord_key = Stamps.stamp
				       val compare = Stamps.compare)
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
 (*
      fun eqTycon(T.NoTP tc, T.NoTP tc') = TU.equalTycon(tc,tc')
	| eqTycon _ = raise Fail "Unimplemented"

      fun eqTycPath(T.TP_VAR x, T.TP_VAR x') = 
	  (case (x, x')
	    of (v1 as {tdepth, num, kind}, 
		v2 as {tdepth=tdepth', num=num', kind=kind'}) =>
	       if DI.eq(tdepth,tdepth') andalso 
		  num = num' andalso LT.tk_eqv(kind, kind') 
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
  *)

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
			      (debugmsg ("--repEPs add stamp "^
					 Stamps.toShortString s'^
					 " to stmpseen");
			       (case (EPMap.find(renv, s),
				     StampSet.member(stmpseen,s')) 
				of ((_, false) | (NONE, _)) => 
				   ep::loop(rest, env, 
					    EPMap.insert(renv,s,ep),
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
			      (presumably a FORMAL or FLEXTYC) *)
			(case tyc 
			   of TP.GENtyc{stamp=s,kind,...} =>
				(case kind 
				   of TP.DATATYPE _ => 
				        loop(rest, env, renv, stmpseen)
				    | _ => proc s)
			    | TP.DEFtyc _ => loop(rest,env, renv, stmpseen)
			    | _ => bug "repEPs 0")
		      | M.STRent _ => bug "repEPs 1"
		      | M.ERRORent => (* in MLRISC/ra/risc-ra.sml this actually happens *)
			loop(rest,env,renv, stmpseen)
		   handle EE.Unbound => bug ("repEPs Unbound"^
					     EP.entPathToString ep)
		end
	in loop(eps, env, EPMap.empty, StampSet.empty)	
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
		      of M.TYCent(TP.GENtyc{kind=TP.DATATYPE _, ...}) =>
			 loop(eps, fsigs, eenv)
		       | M.TYCent(TP.GENtyc{kind, arity, ...}) =>
			 (* Use this when PK eliminated from front-end:
	                    (LT.tkc_int arity)::loop(eps, pfsigs) *)
			 (buildKind arity)::loop(eps, fsigs, eenv)
		       | M.FCTent{paramRlzn, bodyRlzn (* DELETE *), 
				  closure=M.CLOSURE{env, ...}, 
				  ...} =>
                         (* This should be using closure and paramRlzn --
			  * bodyRlzn is being deleted *)
			 (case fsigs 
			   of [] => bug "kinds.1"
			    | fsig::rest => 
			      kinds(#entities paramRlzn, 
				    #entities bodyRlzn, fsig)::
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
		      of M.TYCent(TP.GENtyc{kind=TP.DATATYPE _, ...}) =>
			 loopkind(eps, fsigs, eenv)
		       | M.TYCent(TP.GENtyc{kind, arity, ...}) =>
			 (* Use this when PK eliminated from front-end:
	                    (LT.tkc_int arity)::loop(eps, pfsigs) *)
			 (buildKind arity)::loopkind(eps, fsigs, eenv)
		       | M.FCTent{paramRlzn, bodyRlzn (* DELETE *), 
				  closure=M.CLOSURE{env, ...}, 
				  ...} =>
                         (* this should be using closure and paramRlzn *)
			 (case fsigs 
			   of [] => bug "kinds.1"
			    | fsig::rest => 
			      kinds(#entities paramRlzn, 
				    #entities bodyRlzn, fsig)::
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
		    (* kinds(paramEnts, bodyEnts, fsig) *)
		val fctvar = T.TP_VAR{tdepth=d, num=i, kind=kind}
		val _ = (debugmsg ("--formalBody elements ");
			 if !debugging then ppSig msig else ())
	
		val eps = entpaths(elements)
		val _ = debugmsg ("--formalBody eps "^Int.toString (length eps))
		fun loop(ftmap, eenv, [], i, tps) = (ftmap, rev tps)
		  | loop(ftmap, eenv, ep::rest, i, tps) = 
		    (case EE.lookEP(eenv, ep)
		      of M.TYCent(TP.GENtyc{kind=TP.DATATYPE _, stamp, ...}) =>
			 let val _ = debugmsg ("--formalBody DATATYPE "^
					       Stamps.toShortString stamp)
			 in loop(ftmap, eenv, rest, i, tps) 
			 end
		       | M.TYCent(TP.GENtyc{stamp, kind, arity, ...}) =>
			 let val tp = T.TP_SEL(T.TP_APP(fctvar, argTps), i)
			     val _ = debugmsg ("--formalBody "^
					       Stamps.toShortString stamp^
					       " is index "^
					       Int.toString i)
			 in case FTM.find(ftmap, stamp)
			     of SOME _ => loop(ftmap, eenv, rest, i, tps)
			      | NONE => loop(insertMap(ftmap, stamp, tp),
					     eenv,
				 rest, i+1, tp::tps)
			 end
		       | M.TYCent _ => 
			 (debugmsg "--formalBody other TYCent GEN";
			  loop(ftmap, eenv, rest, i, tps))
		       | M.FCTent _ =>
			 (debugmsg "--formalBody FCTent";
			  loop(ftmap, eenv, rest, i, tps))
		       (* | M.STRent{entities,...} =>
			 (debugmsg "--formalBody STRent";
			  loop(ftmap, eenv,  
			       (#2 (ListPair.unzip (EE.toList entities)))@rest,
			       i, tps)) *)
		       | _ => (debugmsg "--formalBody other ent";
			       loop(ftmap, eenv, rest, i, tps)))
		(* val bodyentsflat = #2 (ListPair.unzip (EE.toList bodyEnts)) *)
		    val (ftmap1, tps) = loop(ftmap0, bodyEnts, beps, 0, [])
		(* val _ = debugmsg ("--formalBody bodyents "^
				  Int.toString(length bodyentsflat))*)
		(* val (ftmap1, tps) = loop(ftmap0, bodyentsflat, 0, []) *)
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
			    rlzn: M.strEntity, M.SIG (sign : M.sigrec), d) =
	    let
		val fsigs = fsigInElems(#elements sign)
		val _  = debugmsg ("--pri num of fsigs "^
				   Int.toString (length fsigs)
				   ^" depth "^DI.dp_print d)
		val entenv = #entities rlzn
		val eps = repEPs(entpaths(#elements sign), #entities freerlzn) 
		(* val eps' = 
		    let 
			fun flatten((stamp, M.STRent{entities,...})::rest) =
			    (map (fn ep => stamp::ep) 
				(flatten(EE.toList entities))) @ flatten(rest)
			  | flatten((stamp, M.TYCent(TP.GENtyc _))::rest) =
			    [stamp]::flatten(rest)
			  | flatten((stamp, (M.FCTent _))::rest) =
			    [stamp]::flatten(rest)
			  | flatten(_::rest) =
			    flatten(rest)
			  | flatten [] = []
		    in flatten (EE.toList (#entities freerlzn))
		    end 
		val _ = (debugmsg "---pri selected eps";
			 if !debugging 
			 then (app (fn x => print((EP.entPathToString x)^";")) 
				  eps; print "\n")
			 else ();
			 debugmsg "\n---pri selected eps'";
			 if !debugging 
			 then (app (fn x => print ((EP.entPathToString x)^";"))
			          eps'; print "\n")
			 else ()) *)
		(* val eps = eps' *)
		
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
		      of M.TYCent(tyc as TP.GENtyc{kind=TP.DATATYPE _, stamp,...}) =>
			   let val tp = T.TP_TYC(T.NoTP tyc)
			       (* val _ = debugmsg "TYCent DATATYPE" *)
			   in (loop(insertMap(ftmap, stamp, tp), 
				    tp::tps, entenv, rest, i+1, fs))
			   end
			   (* Datatypes should be represented directly in the 
			      tycpath *)
		       | M.TYCent(TP.GENtyc{kind=TP.ABSTRACT(tyc),stamp=s1,...}) =>
			   let val (tp,s) = 
				    (case tyc 
			     of TP.GENtyc{kind=TP.DATATYPE _,stamp,...} =>
				(T.TP_TYC(T.NoTP tyc), stamp)
			      | TP.GENtyc{kind=TP.FORMAL, arity, stamp, ...} => 
				(case FTM.find(ftmap, stamp)
				  of SOME tp' => (tp', stamp)
				   | NONE => 
				     (debugmsg ("--eps VAR depth "^DI.dp_print d);
				      (T.TP_VAR{tdepth=d,num=i,
					 kind=buildKind arity}, stamp)))
			      | _ => 
				(debugmsg "--pri[GEN] nonformal/data abstract";
				 (T.TP_TYC(T.NoTP tyc), s1)))
			    in 
			       loop(insertMap(ftmap, s, tp), 
				    tp::tps, entenv,rest,i+1,fs)
			    end
		      
		       | M.TYCent(TP.GENtyc{kind, arity, stamp, ...}) =>
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
				       T.TP_VAR {tdepth=d,num=i, kind=kind}))
			       (* val _ = checkTycPath(tp, tp') *)
			   in 
			     loop(insertMap(ftmap, stamp, tp'),
				  tp'::tps, entenv, rest, i+1, fs)
			   end
		       | M.TYCent tyc => 
			    (debugmsg "--primaryCompInStruct[TYCent]";
			     (let val tp = T.TP_TYC(T.NoTP tyc)
			      in loop(insertMap(ftmap, ev, tp),
				      tp::tps, entenv, rest, i+1, fs)
		              end))
		       | M.FCTent {stamp, paramRlzn, bodyRlzn (* DELETE *), 
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
				     val bodyEnts = #entities bodyRlzn
				     val _ = 
					 if !debugging then 
					     (print "\n===FCTent paramEnts===\n";
					      ppEntities paramEnts;
					      print "\n===FCTent bodyEnts===\n";
					      ppEntities bodyEnts;
					      (* print "\n===FCTent eenv===\n";
					      ppEntities entenv; *)
					      print "\n===FCTent closenv===\n";
					      ppEntities closenv;
					      print "\n--kinds[FCTent] Funsig\n"
					      (* ; ppFunsig fsig; 
					      (* print "\n===FCTent sign===\n";
					      ppSig (M.SIG sign); *)print "\n"*)
					      )
					 else ()

				     val argRepEPs = 
					 repEPs(entpaths(#elements psr),
						#entities paramRlzn)
				     (* val psr = {stamp = Stamps.special "bogusSig",
	 name=NONE, closed=true, fctflag=false,
	 elements=[],
	 properties = PropList.newHolder (),
	 (* boundeps=ref NONE, lambdaty=ref NONE *)
	 typsharing=[], strsharing=[],
	 stub = NONE} *)
				     val (ftmap1, argtps) = 
					 primaryCompInStruct(ftmap,
							     paramRlzn,
							     paramRlzn,
							     paramsig,
							     DI.next d)

				    
                                     (* [TODO] Replace free instantiation
				        components with actual argument 
					realization components. *)
				     (*val knds = kinds(paramEnts, 
						      #entities bodyRlzn,
						      fsig)    
				     val _ = debugmsg "<<kinds done\n" *)

				     (* Can't do normal flextyc tycpath 
				        construction here because actual 
					argument is not yet available.
					Must traverse bodyRlzn in signature
					order and add TP_SEL(TC_APP(tv,args),i)
					for each FORMAL GENtyc *)
				     val _ = debugmsg ("pri[FCT]TP_VAR depth "^
						       DI.dp_print d)
				     (* val fsig = 
					 M.FSIG{kind=NONE,
						paramsig=M.SIG psr,
						paramvar=Stamps.special "bogusP",
						paramsym=NONE,
						bodysig=M.SIG psr}
				     val bsig = M.SIG psr *)
			       (* BOGUS! bodyRlzn is being DELETED *)
				     val (ftmap2,bodytps) = 
					 formalBody(ftmap1, #entities bodyRlzn,
						    argtps, bsig, 
						    (* T.TP_VAR{tdepth=d, 
							     num=i,
							     kind=knds
							     } *)
						    paramEnts,
						    fsig, d, i)
				     val tp' = T.TP_FCT(argtps, bodytps)
				(* val _ = checkTycPath(tp, tp') *)
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
		

	fun getTk(M.FSIG{paramsig=paramsig as M.SIG ps, ...}, dummyRlzn, 
		  argRlzn, 
		  d) =
	    let 
		val _ = debugmsg ">>getTk"
		val (ftmap, argtycs') = 
		    primaryCompInStruct(FTM.empty, dummyRlzn, argRlzn, 
					paramsig, d)
		val _ = debugmsg "<<getTk"
	    in (argtycs', ftmap)
	    end (* getTk *)
	  | getTk _ = bug "getTk 0"

    end (* local *)
end (* local *)
	
end (* structure RepTycProps *)
