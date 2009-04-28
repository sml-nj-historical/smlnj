(* modkind.sml *)

structure ModKind =
struct

(*** computing the TycKind for a particular functor signature ***)
fun getTkFct{sign as M.FSIG{paramvar, paramsig as SIG _, bodysig as SIG_, ...},
	     entEnv, rpath, compInfo as {mkStamp, ...} : EU.compInfo} = 
    let val region=SourceMap.nullRegion
	val (rlzn, _, _, args, _) = 
            instGeneric{sign=paramsig, instKind=INST_FORMAL, entEnv=entEnv,
			rpath=rpath, region=region, compInfo=compInfo}
        val entEnv' = 
            EE.mark(mkStamp, 
                    EE.bind(paramvar, STRent rlzn, entEnv))
        val (_, _, _, res, _) = 
            instGeneric{sign=bodysig, instKind=INST_FORMAL, entEnv=entEnv', 
                        rpath=rpath, region=region, compInfo=compInfo}

        (* calculate the tkinds of the formal components in args and res *)
        (* for tycons, this is based on the arity.
        (* for formal functor components, we have to recurse *)

        val arg_tks = map entToKind args
        val res_tks = map entToKind res

       in Param.tkc_fun(arg_tks, Param.tkc_seq res_tks)
      end

  | getTkFct _ = Param.tkc_fun([], Param.tkc_seq [])

fun entToK ...

end (* structure ModKind *)
