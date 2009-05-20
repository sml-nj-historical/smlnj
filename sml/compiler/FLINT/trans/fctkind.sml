(* fctkind.sml *)

(* Compute a kind for a formal functor element from an instantiated signature.
 * This is a new implementation of the function performed by getTkFct within
 * the old version of instantiate.sml, where is was used to compute the tkind
 * of a functor element for the tycpaths returned by instParam.  This in turn
 * was used in elabmod.sml for the argtycs field of a FCTfct, and also in
 * sigmatch.sml (matchFct1). *)

signature FCTKIND =
sig
  val fsigToTkind : CompInfo.compInfo -> Modules.fctsig * EntityEnv.entityEnv
		    -> PLambdaType.tkind
end

structure FctKind : FCTKIND =
struct

structure M = Modules
structure EP = EntPath
structure PT = PLambdaType

fun tycToKind tyc = PT.tkc_int(TypesUtil.tyconArity tyc)

fun getEntEnv (entities: EntityEnv.entityEnv,[]) = entities  (* top-level functor element *)
  | getEntEnv (entities, ep) = EE.lookStrEP(entities,take(length ep - 1,ep))
  
(*** computing the TycKind for a functor signature ***)

(* bind compInfo locally-global, so we don't have to thread it through 
 * the recursive calls *)
(* not worrying about memoizing results in the functor signature yet *)

fun fsigToKnd compInfo = 
let fun fsigToKnd'{sign as M.FSIG{paramvar, paramsig as SIG _, bodysig as SIG _, ...},
		   entEnv) = 
    let val region=SourceMap.nullRegion  (* dummy region, required by instFormal *)
        val rpath=InvPath.empty (* dummy rpath, required by instFormal *)
	val {rlzn=paramRlzn, primaries=(parTycs,parFcts)} = 
            instFormal{sign=paramsig, entEnv=entEnv,
		       rpath=rpath, region=region, compInfo=compInfo}

        val entEnv' = EE.bind(paramvar, STRent paramRlzn, entEnv))

        val {rlzn=bodyRlzn, primaries=(bodyTycs,bodyFcts)} =
            instFormal{sign=bodysig, entEnv=entEnv', 
                       rpath=rpath, region=region, compInfo=compInfo}

        (* calculate the tkinds of the formal components in argeps and bodyeps
         * for tycons, this is based on the arity.
         * for formal functor components, we have to recurse *)

        (* can directly compute the tyc kinds from the primary tycs *)
        val paramTycTks = map (tycToKind) parTycs
        val bodyTycTks = map (tycToKind) bodyTycs

        (* for primary fcts in param and body, we need to pass appropriate
         * entEnvs, providing the right context for the fsig.  This will be
         * the entities field of the rlzn of the immediately enclosing str. *)
        val parFctTks = map (entPathToKind(paramRlzn)) paramFcts
        val bodyFctTks = map (entPathToKind(bodyRlzn)) bodyFcts

     in PT.tkc_fun(parTycTks@parFctTks, PT.tkc_seq (bodyTycTks@bodyFctTycs)
    end
  | fsigToKnd' _ = PT.tkc_fun([], PT.tkc_seq [])
      (* one of paramsig or bodysig is ERRORsig *)

and entPathToKind ({entities,...}: M.strEntity)
		  (_, fsig, entPath) =
    (* 1. look up the entPath in the signature (how?).
       2. if the entity determined by the entPath is *)
    fsigToKnd'(fsig,getEntEnv(entities,entPath))
    
 in fsigToKnd'
end

fun primaryToBind (compInfo, entEnv: EE.entEnv)
		  (PrimaryTyc arity, stamp, _) =
    (stamp, LT.tkc_int arity)
  | primaryToBind (compInfo,entEnv) (PrimaryFct fsig, stamp, ep)) =
    (stamp, fsigToKnd compInfo (fsig, getEntEnv(entEnv,ep)))
  | primaryToBind (compInfo,entEnv) _ = bug "primaryToKnd"

end (* structure FctKind *)




(* some code from an earlier version that might be useful somewhere...
Navigating a signature via an entPath.

fun matchEV(ev, s as (TYCspec{entVar,...} | STRspec{entVar,...} | FCTspec{entVar,...})) =
       EP.eqEntVar(ev,entVar)
  | matchEv _ = false

fun getSpecEntVar(ev, elements) =
    let fun loop nil = NONE
          | loop ((sym,spec)::elems) = 
	    if matchEV(ev,spec) then SOME spec else loop elems
     in loop elements
    end

(* ASSERT: final spec should be either TYCspec or FCTspec *)
fun getSpecEntPath([ev], elements) = getSpecEntVar(ev,elements)
  | getSpecEntPath(ev::evs, elements) =
    (case getSpecEntVar(ev,elements)
      of SOME(STRspec{sign=SIG{elements=elements',...},...}) =>
	   getSpecEntPath(evs,elements')
       | NONE => NONE)

fun specToTkind (TYCspec{info=RegTycSpec{spec,...},...}) = PL.tkc_int(tyconArity spec)
  | specToTkind (TYCspec{info=InfTycSpec{arity,...},...}) = PL.tkc_int arity
  | specToTkind (FCTspec{sign,...}) = fsigToTkind(sign, entEnv?, rpath?, compInfo)

*)
