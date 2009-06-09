(* modkind.sml *)

(* Compute a kind for a formal functor element from an instantiated signature.
 * This is a new implementation of the function performed by getTkFct within
 * the old version of instantiate.sml, where is was used to compute the tkind
 * of a functor element for the tycpaths returned by instParam.  This in turn
 * was used in elabmod.sml for the argtycs field of a FCTfct, and also in
 * sigmatch.sml (matchFct1). *)


structure ModKind =
struct

local 

structure EP = EntPath
structure PT = PLambdaType
structure TU = TypesUtil
structure INS = Instantiate
structure EE = EntityEnv

open Modules

in

fun tycsToKind _ = raise Fail "Unimplemented" 

(* what is the reason for passing rpath?  Is it relevant here? *)
(* bind compInfo locally-global, so we don't have to thread it through all
 * the calls? *)
(* not worrying about memoizing results in the functor signature yet *)

fun matchEV(ev, s as (TYCspec{entVar,...} | STRspec{entVar,...} | FCTspec{entVar,...})) =
       EP.eqEntVar(ev,entVar)
  | matchEV _ = false

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
       | NONE => NONE
       | SOME _ => raise Fail "getSpecEntPath: unexpected spec")
  | getSpecEntPath([], _) = raise Fail "getSpecEntPath: empty entpath"

fun specToTkind (TYCspec{info=RegTycSpec{spec,...},...}) = PT.tkc_int(TU.tyconArity spec)
  | specToTkind (TYCspec{info=InfTycSpec{arity,...},...}) = PT.tkc_int arity
  | specToTkind (FCTspec{sign,...}) = raise Fail "Unimplemented" (* fsigToTkind (sign, entEnv(*?*), rpath(*?*), compInfo) *)
  | specToTkind _ = raise Fail "specToTkind: unexpected spec"

(*** computing the TycKind for a functor signature ***)
and fsigToTkind(sign as FSIG{paramvar, paramsig as SIG _, bodysig as SIG_, ...},
		entEnv, rpath, compInfo) = 
    let val region=SourceMap.nullRegion  (* dummy region, required by instFormal *)

	val {rlzn=paramRlzn, primaries=paramPrimaries} = 
            INS.instFormal{sign=paramsig, entEnv=entEnv,
		       rpath=rpath, region=region, compInfo=compInfo}

        val entEnv' = EE.bind(paramvar, STRent paramRlzn, entEnv)

        val {primaries=bodyPrimaries, ...} =
            INS.instFormal{sign=bodysig, entEnv=entEnv', 
                       rpath=rpath, region=region, compInfo=compInfo}

        (* calculate the tkinds of the formal components in argeps and bodyeps
         * for tycons, this is based on the arity.
         * for formal functor components, we have to recurse *)

        val paramTks = raise Fail "Unimplemented" (* map (entToKind paramsig) paramEps *)
        val bodyTks = raise Fail "Unimplemented" (* map (entToKind bodysig) bodyEps *)

       in PT.tkc_fun(paramTks, PT.tkc_seq bodyTks)
      end

  | fsigToTkind _ = PT.tkc_fun([], PT.tkc_seq [])
      (* one of paramsig or bodysig is ERRORsig *)

and entToKind (sign as SIG{elements,...}) entPath =
    (* 1. look up the entPath in the signature (how?).
       2. if the entity determined by the entPath is *)
    specToTkind(Option.valOf (getSpecEntPath(entPath, elements)))
  | entToKind _ _ = raise Fail "entToKind: unexpected entity"

end (* local *)    
end (* structure ModKind *)
