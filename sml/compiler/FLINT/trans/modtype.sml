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
		    -> PLambdaType.tyc list   (* or .lty list ? *)
   val getFctTyc : Modules.fctSig * Modules.fctEntity * TransTypes.primaryEnv
		   * Absyn.dec CompInfo.compInfo
		   -> PLambdaType.tyc  (* or .lty ? *)
end

structure ModType : MODTYPE =
struct 
	
local
  structure M = Modules
  structure T = Types
  structure TU = TypesUtil
  structure TP = TycPath
  structure EE = EntityEnv
  structure EP = EntPath
  structure EV = EvalEntity
  structure SE = StaticEnv
  structure LT = LtyExtern
  structure INS = Instantiate
  structure IP = InvPath
  structure TT = TransTypes
  structure PP = PrettyPrintNew
		
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
fun getStrTycs(primaries, entities: EE.entityEnv, penv: primaryEnv, compInfo)
    : LT.tyc list =
    let fun transPrimary (primsig,_,ep) = 
	    let val ent = EE.lookEP(entities, ep)  (* fetch the primary entity at ep *)
	     in case ent
		 of M.TYCent tyc => (* tyc could be formal or nonformal *)
		    (case primsig
		      of M.PrimaryTcy n => TT.tyconToTyc(tyc, penv, penvDepth(penv))
		          (* We assume arity will match - could check this. *)
		       | _ => bug "getStrTyc 1")
		  | M.FCTent fctEnt =>
		    (case primsig
		      of M.PrimaryFct fctsig => getFctTyc(fctsig,fctEnt,penv,compInfo)
		       | _ => bug "getStrTyc 2")
		  | M.STRent _ => bug "getStrTycs -- STRent"
		  | M.ERRORent => bug "getStrTycs -- ERRORent"
	    end

     in map transPrimary primaries
    end
(*

In the TYCent case, tyconToTyc will have to search for tyc in penv and
if it finds it in penv it will translate to a corresponding tcc_var;
otherwise it translates tyc directly.  What forms of tycons will be
found in penv?  Only FORMAL? Yes, because they will have come from a
functor parameter formal instantiation at some surrounding abstraction
level. This corresponds to the FLEXTYC case in the old getTycPaths
function.  Non-FORMAL tycons and functors can also occur, of course,
because entities can be a realization for (e.g.) the coerced actual
parameter structure in a functor application. Do we ever apply
getStrTycs to a formal instantiation realization for a signature? For
such a realization, the primary tycons would be genTyc[FORMAL] and the
primary functors will be formal functors. Yes, this could presumably
happen if a outer functor parameter was passed directly as a parameter
to a functor application within the parameter's scope.

So in tyconToTyc we don't need to search penv unless the tycon is a
genTyc[FORMAL].  Similarly, in the FCTent case, getFctTyc does not
need to search penv for the functor unless we know it is formal
(i.e. an instantiated fctsig with FORMstr body).

There is a special case where the structure was the result of an
application of a formal parameter functor (within a functor body). In
this case result primaries should be expressed as projections
(LT.tcc_proj, formerly TP_SEL) of a tyc that is the application
(formerly TP_APP) of the functor LT.tyc (a tcc_var) to the argument
LT.tyc.

How do we detect this kind of structure after the fact, and
how to we recover the needed functor an argument tycs?  In the old
system, the tycpaths were computed in instFMBD when the functor
application was evaluated in evalent.sml.  Do we need to process the
functor body entity expression?

The above getStrTycs should work for non-volatile (top-level) structures,
but for volatile structures defined in a functor context it seems we will
have to "type-check" the functor body entity expression, probably using
information from signatures.

*)



(* function that types a functor body entDecl, producing an ltycEnv *)

datatype ltycTree
  = TYCleaf of ltyc
  | FCTleaf of ltyc  (* is the FLINT type of a functor an LT.tyc or LT.lty? *)
  | STRnode of ltycEnv  (* don't need ltycs for structures *)

withtype ltycEnv = (entvar * ltyctree) list

fun typeBodyDecl (entDec, sign, ltycenv, penv) : ltycEnv =
    (* should entDec return a cumulative or incremental ltycEnv? *)

fun tycExpTyc (tycExp, ltycenv) : LT.tyc =
    case tycExp
     of VARtyc entpath => lookPathLtyc(ltycenv, entpath)
      | CONSTtyc tycon =>
	(* nonvolatile tycon, not relativized, won't contain
	 * any PATHtycs or volatile primaries *)
	TT.tyconToTyc(tycon, emptyPenv, 0, emptyLtycEnv)
      | FORMtyc tycon => (* lookup in penv? same as CONSTtyc? *)  
	(* tycon is a formalized (i.e. relativized) variant
	 * of a declared tycon, either DEFtyc or datatype.
	 * have to pass ltycEnv for looking up entpaths.
	 * ASSERT: Don't need penv because any free occurrences
	 * of volatile primaries will have been relativized,
	 * and so can be translated via ltycenv. *)
	TT.tyconToTyc(tycon, emptyPenv, 0, ltycenv)

fun entDec (TYCdec(ev,tycExp), ltycenv) : ltycEnv = 
    let val ltyc =  tycExpTyc(tycExp, ltycenv) (* tycon leaf *) 
     in [(ev, TYCleaf ltyc)]
    end

  | entDec (STRdec(ev,strExp,_), ltycenv) =  (* structure node *)
    (* we don't need an ltyc for the structure itself, just a nested
     * ltycEnv for its contents *)
    let val strltycenv = strExpLtycEnv(strExp, ...) \
     in [(ev, STRnode strltycenv)]
    end

  | entDec (FCTdec(ev,fctExp), ltycenv) =  (* functor leaf *)
    let val ltyc = fctExpTyc(fctExp, ltycenv, ...) 
     in [(ev, FCTleaf ltyc)]
    end

  | entDec (SEQdec entDecs, ltycenv) =
    foldl (fn (entDec, ltycenv') => entDec(entDec, ltycenv'@ltycenv)@ltycenv) nil entDecs

  | entDec (LOCALdec (locDecs, bodyDecs), ltycenv) =
    entDec(bodyDecs, entDec(locDecs, ltycenv) @ ltycenv)

  | entDec (EMPTYdec, ltycenv) = []

  | entDec (ERRORdec, _) = bug "typeEntDecl ERRORdec"

	    
and strExpTycs (strExp, primaries, ...) : LT.tyc list =
    (* computer ltycenv for strExp, then extract LT.tycs for primaries *)
    let val strltycenv = strExpLtycEnv (strExp, ...)
     in map (fn (_,_,ep) => lookEntTyc (ep, strltycenv)) primaries
    end

(* fctExpTyc -- map a fctExp to an LT.tyc *)
and fctExpTyc (fctExp, ltycenv, penv, depth, compInfo) : LT.tyc =
    case fctExp
      of VARfct entpath => lookPathLtyc(ltycenv, entpath)

       | CONSTfct (fctEnt, fctsig) =>
	 (* we need a functor sig here.  Should it be stored in the CONSTfct
	  * fctExp, or passed as a parameter to fctExpTyc? *)
	 getFctTyc(fctsig, fctEnt, penv, compInfo?)

       | LAMBDA {param, body, fctsig} =>
	 (* we need a functor sig here.  Should it be stored in the LAMBDA
	  * fctExp, or passed as a parameter to fctExpTyc? *)
	 let val F.FSIG{paramsig,bodysig} = fctsig
	     val {primaries = paramPrimaries, rlzn=paramRlzn} =
		 (* instantiate paramsig to get its primaries *)
		 INS.instFormal{sign=paramsig, entEnv=???,
				rpath=InvPath.IPATH[], compInfo=compInfo,
				region=SourceMap.nullRegion}
	     (* translate the parameter inst. rlzn into a ltycEnv *)
	     val paramLtycEnv = strLtycEnv(paramRlzn, ...)

	     (* construct an inner ltycenv that binds
	      * the param ltycenv to the param entvar *)
	     val innerLtyEnv = (param, STRnode paramLtycEnv) :: ltycenv

	     val bodyLtycEnv = strExpLtyEnv(body, innerLtycEnv, ...)

	     val paramPrimaryKinds =
		 map (#2 o (FctKind.primaryToBind (compInfo, paramEnv)))
		     paramPrimaries

	     (* instantiate bodysig to determine its primaries *)
	     val {primaries = bodyPrimaries, ...} =
		 INS.instFormal{sign=bodysig, entEnv=???,
				rpath=InvPath.IPATH[], compInfo=compInfo,
				region=SourceMap.nullRegion}

	     (* access the LT.tycs for body primaries in bodyLtycEnv *)
	     val resultLtycs = 
		 map (fn (_,_,ep) => lookEntTyc(ep, bodyLtycEnv)) bodyPrimaries

	  in LT.tcc_fn (paramKinds, LT.tcc_seq(resultLtycs))
	 end

       | LETfct (entDec, fctExp, fctsig) =>
	 let val localLtycEnv = entDec(entDec, ltycenv, ...)
	  in fctExpTyc(fctExp, localLtycEnv@ltycenv, penv, depth, compInfo?)
	 end

(* strExpLtycEnv -- map a strExp to an ltycEnv *)
and strExpLtycEnv (strExp, ltycenv, ...) : ltycEnv =
    case strExp
      of VARstr entpath =>
	 (* reference to a previously defined volatile substructure *)
	 (case lookPathLtyc(ltycenv, entpath)
	    of STRnode ltycenv' => ltycenv'
	     | _ => bug "typeBodyDecl STRdec 1")

       | CONSTstr strEnt =>
	 (* nonvolatile constant? or can it be volatile? 
	  * or would a volatile str always be represented by
	  * a VARstr? 
	  * Can we ignore this because none of its elements
	  * can be primaries? No. *)
	 getStrLtycEnv(..., strEnt, ...)

       | STRUCTURE {entDec,...} => entDec(entDec, ltycenv)
	 (* is it ok to return the cumulative ltycEnv here?
	  * Maybe, but let's return an incremental ltycEnv. *)

       | APPLY (fctExp, strExp) =>
	 let val fctLtyc = fctExpTyc(fctExp, ... )
	     val argLtycs = strExpTycs(strExp, ... )
	  in LT.tcc_app(fctLtyc, argLtycs)
	 end

       | LETstr (localDec, strExp) =>
	 let val ent

       | ABSstr (sign, strExp) =>

       | FORMstr fctsig =>  bug "FORMstr"
	  (* can't happen? Shouldn't find FORMstr embedded
	   * within a regular function body.  It can only
	   * occur as the whole functor body, instead of
	   * a STRUCTURE *)

       | CONSTRAINstr {boundvar, raw, coercion } =>
	 (* is this what we always have for a body where
	  * the functor has a result signature? *)
(* end strExpLtycEnv *)

(* getFctTyc : M.fctSig * M.fctEntity * TT.primaryEnv
	       * Absyn.dec CompInfo.compInfo
	       -> PLambdaType.tyc

Returns the LT.tyc representing the functor static action.

Note that the functor being translated can be either a regular functor
or a formal functor (instantiated parameter fctsig). If it is a formal
functor, we look it up in penv, and return a tcc_var as its PT.tyc.
[In the old version, the tcc_var coordinates were stored in a TP_VAR
assigned to the tycpath field of the functor realization in instToEntity
FinalFct case in instParam.]

For a regular functor (where the body is not a FORMstr), we use an
algorithm based on that used in old sigmatch to compute tycpath field of the
functor realizations resulting from a fctsig match (in matchFct1).
*)

and getFctTyc(fctsig, fctEntity: M.fctEntity, penv: primaryEnv, compInfo) =
    let 
	val M.FSIG{paramsig, bodysig} = fctsig
            (* need bodysig to calculate primaries for result structure *)

	val {primaries, paramRlzn, exp, closureEnv, ...} = fctEntity
	    (* paramRlzn was paramEnv, just the entities field of paramRlzn.
	     * Could recreate primaries and paramRlzn at this point by re-instantiating
	     * paramsig, since entities and stamps from the original
	     * parameter instantiation during functor elaboration would
	     * have been relativized away. *)

        (* how do we use the closureEnv? Presumably we need it to interpret
	 * PATHtyc's somewhere, undoing relativizations. *)

        (* check if fctEntity is formal, and if so, look it up in penv to
	 * get tcc_var coordinates. (obsolete because this function will not
	 * be used for formal functors?) *)

	val M.LAMBDA {param, body} = exp
            (* need param field to define bodyEnv below *)
	val bodyPrimaries = (* get primaries for bodysig, presumably by
			     * instantiation *)

     in case body
	  of M.STRUCTURE{entDec,...} =>  (* regular functor *)
	     let val {entities, ...} = paramRlzn
		 val paramLtycEnv : ltycEnv = typeEntities entities 
                   (* compute types of param entities -- all of them,
		    * not just primaries (?) *)
	         val ltenvParam = bindLtyc(param, STRtenv paramLtycEnv)
	           (* intial ltyc env binds param entvar. 
		    * Do we need this, or just innerPenv defined below? *)
		 val innerPenv = paramPrimaries :: penv
		   (* penv for processing body *)
		 val ltenvBody = typeEntDecl(entDec, ltenvParam, innerPenv)
	           (* how do we use parameter primaries? Presumably these
		    * should translate to tcc_var tycs in paramLtyEnv? Or does this
		    * get taken care of when translating tycons against penv? *)
		 val bodyLtyc = LT.tcc_str(getPrimaries(bodyPrimaries, ltenvBody))
	           (* extract body primaries from ltenvBody *)
		 val paramTkinds = map getTkind paramPrimaries
	      in LT.tcc_fun(paramTkinds, bodyLtyc)
	     end

	   | M.FORMstr sign =>
	      (* is sign = bodysig? *) 
	      (* The ltyc of a formal functor should be a tcc_var, since a formal
	       * functor is a primary of a functor parameter. This tcc_var
	       * is external to the formal functor and so can't be computed
	       * here. *)

(* old stuff, before typing body entDecl -----------------------------
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
------------------------------------------- *)

    end

end (* local *)
end (* structure ModTypes *)
