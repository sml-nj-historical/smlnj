(* coreacc.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure CoreAccess : sig
    exception NoCore
    val getExn : StaticEnv.staticEnv * string -> VarCon.datacon
    val getVar : StaticEnv.staticEnv * string -> VarCon.var
end = struct
    exception NoCore
    fun dummyErr _ _ _ = raise NoCore
    fun path name = SymPath.SPATH [CoreSym.coreSym,Symbol.varSymbol name]

    fun getExn (env, name) =
	(case Lookup.lookVal (env, path name, dummyErr) of
	     VarCon.CON x => x
	   | _ => VarCon.bogusEXN)
	handle NoCore => VarCon.bogusEXN

    fun getVar (env, name) =
	(case Lookup.lookVal (env, path name, dummyErr) of
	     VarCon.VAL v => v
	   | _ => raise NoCore)
	handle NoCore =>
	       raise Fail ("CoreAccess.getVar: cannot access " ^ name)
end
