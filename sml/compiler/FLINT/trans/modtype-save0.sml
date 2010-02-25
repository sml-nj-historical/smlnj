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


(* getFctTyc : M.fctSig * M.fctEntity * TT.primaryEnv
	       * Absyn.dec CompInfo.compInfo
	       -> PLambdaType.tyc
*)
(*

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

end (* local *)
end (* structure ModTypes *)
