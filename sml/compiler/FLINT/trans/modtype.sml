(* modtype.sml

   This module processes the static information (realizations and signatures)
   to obtain primary type component information for Translate. It fills in 
   tycpath and sigBoundeps information.

   sigBoundeps is a list of the representative entities for a signature
   This is used to determine which tycs in a functor parameter are 
   representative


   Datatype bindings must be accounted for in Tycpath computations 
   because they will be represented as TP_TYC(...).  
*)

signature MODTYPE =
sig
   val strType : Modules.Signature * Modules.strEntity ... -> PLambdaType.tyc
   val fctType : Modules.fctsig * Modules.fctEntity ... -> PLambdaType.tyc
end

structure ModType : MODTYPE =
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
		
  (* A StampSet ADT to keep track of unique stamps (embedded in different
   * structures) we have seen *)
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
	  (* What is the correct eenv to look up belem entpaths? *)
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
		 | M.FCTent{exp=M.LAMBDA{paramRlzn,...}, closureEnv=env, ...} =>
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
	  val _ = debugmsg "--formalBody paramtk computed"

	  (* [TODO] This can be a problem. belems can refer to 
	     formal functor body for curried functor,
	     but formal body signature has not been
	     instantiated with the actual argument realization. *)
	  val beps = repEPs(entpaths belems, bodyEnts)
	  val _ = debugmsg "--formalBody beps computed\n"
	  val bfsigs = fsigInElems belems
	  val _ = debugmsg "--formalBody bfsigs computed\n"
	  (* What is the correct eenv to look up belem entpaths? *)

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

    FTM.map * M.strEntity * M.strEntity option * M.Signature * DI.depth
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
	  fun loop(ftmap, tps, [], _, _) = (ftmap, rev tps)
	    | loop(ftmap, tps, ep::rest, i, fs) =
	      
	       let val _ = debugmsg ("-primaryCompInStruct loop "^Int.toString i)
		   val ev : Stamps.stamp = List.last ep
	       in case EE.lookEP(entenv, ep)
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
					(debugmsg ("--eps VAR depth "^DI.dp_print d);
					 (TP.TP_VAR{tdepth=d,num=i,
						    kind=buildKind arity}, stamp)))
				 | _ => 
				   (debugmsg "--pri[GEN] nonformal/data abstract";
				    (TP.TP_TYC(tyc), s1)))
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
      end (* fun primaryCompInStruct *)

    | primaryCompInStruct _ = bug "Unexpected error signature"

end (* local *)
	
end (* structure ModTypes *)
