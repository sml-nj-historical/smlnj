(* modkind.sml *)

(* Compute a kind for a formal functor element from an instantiated signature.
 * This is a new implementation of the function performed by getTkFct within
 * the old version of instantiate.sml, where is was used to compute the tkind
 * of a functor element for the tycpaths returned by instParam.  This in turn
 * was used in elabmod.sml for the argtycs field of a FCTfct, and also in
 * sigmatch.sml (matchFct1). *)


structure ModKind =
struct

structure EP = EntPath
structure PT = PLambdaType

(* what is the reason for passing rpath?  Is it relevant here? *)
(* bind compInfo locally-global, so we don't have to thread it through all
 * the calls? *)
(* not worrying about memoizing results in the functor signature yet *)

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

(*** computing the TycKind for a functor signature ***)
fun fsigToTkind{sign as M.FSIG{paramvar, paramsig as SIG _, bodysig as SIG_, ...},
		entEnv, rpath, compInfo) = 
    let val region=SourceMap.nullRegion  (* dummy region, required by instFormal *)

	val {rlzn=paramRlzn, tyceps=paramEps, ...} = 
            instFormal{sign=paramsig, entEnv=entEnv,
		       rpath=rpath, region=region, compInfo=compInfo}

        val entEnv' = EE.bind(paramvar, STRent paramRlzn, entEnv))

        val {tyceps=bodyEps, ...} =
            instFormal{sign=bodysig, entEnv=entEnv', 
                       rpath=rpath, region=region, compInfo=compInfo}

        (* calculate the tkinds of the formal components in argeps and bodyeps
         * for tycons, this is based on the arity.
         * for formal functor components, we have to recurse *)

        val paramTks = map (entToKind paramsig) paramEps
        val bodyTks = map (entToKind bodysig) bodyEps

       in PT.tkc_fun(paramTks, PT.tkc_seq bodyTks)
      end

  | fsigToTkind _ = PT.tkc_fun([], PT.tkc_seq [])
      (* one of paramsig or bodysig is ERRORsig *)

and entToKind (sign as SIG{elements,...}) entPath =
    (* 1. look up the entPath in the signature (how?).
       2. if the entity determined by the entPath is *)
    specToTkind(getSpecEntPath(entPath, elements))
    
end (* structure ModKind *)
