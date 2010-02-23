(* modtype.sml

   This module processes the static information (realizations and signatures)
   to obtain primary type component information for Translate.

   [Datatype bindings must be accounted for in Tycpath computations 
   because they will be represented as TP_TYC(...).]
*)

signature MODTYPE =
sig
   val getStrTycs : Modules.primary list * EntityEnv.entityEnv
		    * TransTypes.primaryEnv * Absyn.dec CompInfo.compInfo 
		    -> PLambdaType.tyc list
   val getFctTyc : Modules.fctSig * Modules.fctEntity * TransTypes.primaryEnv
		   * Absyn.dec CompInfo.compInfo
		   -> PLambdaType.tyc
end

structure ModType : MODTYPE =
struct 
	
local
  structure M = Modules
  structure T = Types
  structure TP = TycPath
  structure EE = EntityEnv
  structure EP = EntPath
  structure EV = EvalEntity
  structure SE = StaticEnv
  structure PP = PrettyPrintNew
  structure DI = DebIndex
  structure LT = LtyExtern
  structure TU = TypesUtil
  structure INS = Instantiate
  structure IP = InvPath
  structure MK = ModKind
  structure TT = TransTypes
		
in

  val debugging = FLINT_Control.trdebugging
  val printStrFct = ref true

  (* Support functions *)
  fun debugmsg(m) = 
      if !debugging then print ("RepTycProps: "^m^"\n") else ()

  fun bug msg = ErrorMsg.impossible("RepTycProps: " ^ msg)

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

  val buildKind = LT.tkc_int

(* getStrTycs : Modules.primary list * EntityEnv.entityEnv
	        * TransTypes.primaryEnv * Absyn.dec CompInfo.compInfo 
		-> PLambdaType.tyc list
 * fetching the list of LT.tycs for the primaries of a structure
 * Modeled on getTycPaths from the old version of Instantiate.
 * assumes primaries are passed as an argument. *)
fun getStrTycs(primaries, entities: EE.entityEnv, penv: primaryEnv, compInfo) =
    let fun getPrimaryTyc (primsig,_,ep) = 
	    let val ent = EE.lookEP(entities, ep)  (* fetch the primary entity at ep *)
	     in case ent
		 of M.TYCent tyc => (* tyc could be formal or nonformal *)
		    (case primsig
		      of M.PrimaryTcy n => TT.tyconToTyc(tyc, penv, penvDepth(penv))
		          (* We assume arity will match - could check this.
			   * tyconToTyc will have to search for tyc in penv and
			   * if it finds it in penv it will translate to a corresponding
			   * tcc_var; otherwise it translates tyc directly.
			   * What forms of tycons will be found in penv?  Only
			   * FORMAL? Yes, because they will have come from a 
			   * functor parameter formal instantiation at some
			   * surrounding abstraction level. This corresponds to the
			   * FLEXTYC case in the old getTycPaths function.
			   * Non-FORMAL tycons and functors can also occur, of course,
			   * because entities can be a realization for
			   * (e.g.) the coerced actual parameter structure in a functor
			   * application. Do we ever apply getStrTycs to a formal
			   * instantiation realization for a signature? For such a
			   * realization, the primary tycons would be genTyc[FORMAL]
			   * and the primary functors will be formal functors. Yes,
			   * this could presumably happen if a outer functor parameter
			   * was passed directly as a parameter to a functor application
			   * within the parameter's scope. *)
		       | _ => bug "getPrimaryTyc 1")
		  | M.FCTent fctEnt =>
		    (case primsig
		      of M.PrimaryFct fctsig => getFctTyc(fctsig,fctEnt,penv,compInfo)
		       | _ => bug "getPrimaryTyc 2")
		  | M.STRent _ => bug "getStrTycs -- STRent"
		  | M.ERRORent => bug "getStrTycs -- ERRORent"
	    end

     in map getPrimaryTyc primaries
    end
(* so in tyconToTyc we don't need to search penv unless the tycon is a
 * genTyc[FORMAL].  Similarly, we don't need to search penv for a functor
 * unless we know the functor is formal (an instantiated fctsig). *)


(* getFctTyc : Modules.fctSig * Modules.fctEntity * TransTypes.primaryEnv
	       * Absyn.dec CompInfo.compInfo
	       -> PLambdaType.tyc
*)
(* based on routine used in old sigmatch to compute tycpath field of the
 * functor entity resulting from a fctsig match. Returns the LT.tyc representing
 * the functor static action.
 * Note that the functor being translated can be either a normal functor
 * or a formal functor.  If it is a formal functor, we need to look it up
 * in penv, and return a tcc_var as its PT.tyc *)
and getFctTyc(fctsig, fctEntity: M.fctEntity, penv: primaryEnv, compInfo) =
    let val {primaries, paramRlzn, exp, closureEnv, ...} = fctEntity
	       (* maybe paramEnv should be a strEntity?  ==> paramRlzn *)
(*
        (*  make paramEnv field of fctEntity be paramRlzn : strEntity *)
	val paramRlzn = (* reconstruct a full parameter rlzn from paramEnv *)
             (* ??  paramEnv ?? (* we need a strEntity for bodyEnv *) *)
*)
        (* check if fctEntity is formal, and if so, look it up in penv to
	 * get tcc_var coordinates. *)

	val M.FSIG{bodysig,...} = fctsig
            (* need bodySig to calculate primaries for result structure *)

	val M.LAMBDA{param,body} = exp
            (* need param field to define bodyEnv below *)

	val resultEnt = evalEnt(APP(fctEntity,paramRlzn), ...) ???
            (* apply the functor to the parameter instantiation, i.e. 
	     * the one that was saved in the fctEntity (= functor realization) *)

	val paramTycs = getStrTycs(primaries,paramEnv,penv,compInfo)
	    (* primaries includes both primary tycons and functors,
	     * these are the argument tycs *)

	val bodyEnv = EE.bind(param, M.STRent paramRlzn, closureEnv)

	val {primaries=resPrimaries, ...} =  (* not using the actual functor? *)
	    INS.instFormal{sign=bodysig, entEnv=bodyEnv, (* the right entEnv? *)
			   rpath=IP.IPATH[], compInfo=compInfo,
			   region=SourceMap.nullRegion}
            (* this looks similar to what we would do with a formal functor to
	     * perform its application, but all we are looking for here is
	     * the primaries for the result structure. [What is this going to
	     * do if the bodysig is extracted?] *)

	val bodyPenv = primaries::penv (* push param primaries on primaryEnv *)

	val resultTycs = getStrTycs(resPrimaries,resultEnt,bodyPenv,compInfo)
            (* this might work for nonformal functors? But can we just use
             * the entities from the formal application, or do we need to
	     * translate the body of the functor entity expression? It seems
	     * that the translation of the evaluated entities may suffice,
	     * but this needs to be checked carefully. *)

            (* the translation of the result primary entities in the case of
	     * a formal functor is special.  We need to construct a tcc application
	     * of the functor's tcc_var (to what arg? -- an innermost tcc_var representing
	     * the formal functor parameter) and then select from the result
	     * of this application (the TP_SEL case from old instantiateParam) *)

	val paramkinds = map MK.tycsToKind paramTycs
           (* or should we calculate the paramkinds directly from primaries *)

     in (* TP_FCT(paramtycs,resPrimaries)  --- translate this to a tyc! *)
        LT.tcc_fn(paramkinds, LT.tcc_seq resultTycs)
    end

(* where do the TP_SEL (=> LT.tcc_proj) tycpaths play a part? *)
 
end (* local *)
end (* structure ModTypes *)


(* old code from reptycprops.sml 

  fun formalBody(ftmap0, bodyEnts, argTps, msig as M.SIG{elements, ...}, 
		 paramEnts, fsig, d, i) =
      let val M.FSIG{paramsig=M.SIG{elements=pelems,...},
		     bodysig=M.SIG{elements=belems,...},...} = fsig
	  val peps = repEPs(entpaths pelems, paramEnts)
	  val pfsigs = fsigInElems pelems

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
		 | M.FCTent{exp=M.LAMBDA{paramRlzn,...}, closureEnv=env, ...} =>
		   (* this should be using closure and paramRlzn *)
		   (case fsigs 
		     of [] => bug "kinds.1"
		      | fsig::rest => 
			kinds(#entities paramRlzn, 
			      (raise Fail "#entities bodyRlzn"), fsig)::
			loopkind(eps, rest, eenv))
		 | _ => bug "kinds.0")

	  val paramtk = loopkind(peps,pfsigs,paramEnts)

	  (* [TODO] This can be a problem. belems can refer to 
	     formal functor body for curried functor,
	     but formal body signature has not been
	     instantiated with the actual argument realization. *)
	  val beps = repEPs(entpaths belems, bodyEnts)
	  val bfsigs = fsigInElems belems
	  (* What is the correct eenv to look up belem entpaths? *)

	  val bodytk = loopkind(beps,bfsigs, bodyEnts)

	  val kind = LT.tkc_fun(paramtk, LT.tkc_seq bodytk)

	  val eps = entpaths(elements)
	  val _ = debugmsg ("--formalBody eps "^Int.toString (length eps))
	  fun loop(ftmap, eenv, [], j, tps) = (ftmap, rev tps)
	    | loop(ftmap, eenv, ep::rest, j, tps) = 
	      (case EE.lookEP(eenv, ep)
		of M.TYCent(T.GENtyc{kind=T.DATATYPE _, stamp, ...}) =>
		   loop(ftmap, eenv, rest, j, tps) 
		 | M.TYCent(T.GENtyc{stamp, ...}) =>
		   let val fctvar = TP.TP_VAR{tdepth=d, num=i, kind=kind}
		       val tp = TP.TP_SEL(TP.TP_APP(fctvar, argTps), j)
		   in case FTM.find(ftmap, stamp)
		       of SOME _ => loop(ftmap, eenv, rest, j, tps)
			| NONE => loop(insertMap(ftmap, stamp, tp), eenv,
				       rest, j+1, tp::tps)
		   end
		 | M.TYCent _ => 
		    loop(ftmap, eenv, rest, j, tps)
		 | M.FCTent _ =>
		    loop(ftmap, eenv, rest, j, tps)
		 | _ => loop(ftmap, eenv, rest, j, tps)))
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

    FTM.map * M.strEntity * M.strEntity option * M.Signature * DI.depth
    -> FTM.map * tycpath list 
     *)  
  (* The goal here, simply put, is to get the primary components
     in rlzn where a component is primary if it is a representative
     picked by instantiate in freerlzn. *)
  fun strType(penv(?): primaryEnv,
	      freerlzn : M.strEntity, 
	      M.SIG (sign : M.sigrec),
	      rlzn as {entities=entenv,...}: M.strEntity,
	      primaries,
	      depth) =
      let val eps = map #3 primaries
	  fun loop(ftmap, tps, [], _, _) = (ftmap, rev tps)
	    | loop(ftmap, tps, ep::rest, i, fs) =
	      (case EE.lookEP(entenv, ep)
		    handle EntityEnv.Unbound =>
			   (print "\npri for Unbound\n";
			    raise EntityEnv.Unbound)
		of M.TYCent(tyc as T.GENtyc{kind=T.DATATYPE _, stamp,...}) =>
		   let val tp = TP.TP_TYC(tyc)
		   in (loop(insertMap(ftmap, stamp, tp), 
			    tp::tps, rest, i+1, fs))
		   end
		    (* Datatypes should be represented directly in the tycpath *)
		 | M.TYCent(T.GENtyc{kind=T.ABSTRACT(tyc),stamp=s1,...}) =>
		   let val (tp,s) = 
			   (case tyc 
			     of T.GENtyc{kind=T.DATATYPE _,stamp,...} =>
				(TP.TP_TYC(tyc), stamp)
			      | T.GENtyc{kind=T.FORMAL, arity, stamp, ...} => 
				(case FTM.find(ftmap, stamp)
				  of SOME tp' => (tp', stamp)
				   | NONE => 
				     (TP.TP_VAR{tdepth=depth,num=i,
						kind=buildKind arity}, stamp))
			      | _ => 
				(TP.TP_TYC(tyc), s1))
		   in loop(insertMap(ftmap, s, tp), 
			   tp::tps, rest, i+1, fs)
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
				of SOME tp' => tp'
				 | NONE => TP.TP_VAR {tdepth=d,num=i, kind=kind})
		      in loop(insertMap(ftmap, stamp, tp'),
			      tp'::tps, rest, i+1, fs)
		      end
		 | M.TYCent tyc => 
		      let val _ = debugmsg "--primaryCompInStruct[TYCent]"
			  val tp = TP.TP_TYC(tyc)
		      in loop(insertMap(ftmap, ev, tp),
			      tp::tps, rest, i+1, fs)
		      end
		 | M.FCTent {stamp, exp=M.LAMBDA{paramRlzn, ...},
				closureEnv=closenv,...} => 
		      (debugmsg "--primaryCompInStruct[FCTent SOME]";
                       (* this should be using closure and paramRlzn *)
		       (case fs
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

			       (* BOGUS! bodyRlzn is being DELETED *)
				val (ftmap2,bodytps) = 
				    formalBody(ftmap1, (raise Fail "#entities bodyRlzn"),
					       argtps, bsig, 
					       paramEnts,
					       fsig, d, i)
				val tp' = TP.TP_FCT(argtps, bodytps)
			    in 
				loop(ftmap2, tp'::tps, rest, i+1, srest)
			    end
			  | _ => bug "unexpected errorFSIG"))
		 | _ => bug "primaryCompInStruct 0"
	       end (* loop *)
	      handle EE.Unbound => bug "primaryCompInStruct Unbound"
       in loop(ftmap0, [], eps, 0, fsigs)
      end (* fun strType *)

    | primaryCompInStruct _ = bug "Unexpected error signature"
*)
