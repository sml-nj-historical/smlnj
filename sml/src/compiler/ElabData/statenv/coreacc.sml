(* coreacc.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure CoreAccess : sig
    val getVar : StaticEnv.staticEnv * string -> VarCon.var
    val getCon : StaticEnv.staticEnv * string -> VarCon.datacon
    val getVar' : (unit -> VarCon.var) ->
		  StaticEnv.staticEnv * string -> VarCon.var
    val getCon' : (unit -> VarCon.datacon) ->
		  StaticEnv.staticEnv * string -> VarCon.datacon

    (* like getCon, but returns a bogus exn instead of failing *)
    val getExn : StaticEnv.staticEnv * string -> VarCon.datacon
end = struct

    local
	exception NoCore
	fun dummyErr _ _ _ = raise NoCore
	fun path name = SymPath.SPATH [CoreSym.coreSym,Symbol.varSymbol name]
	fun getCore (env, s) = Lookup.lookVal (env, path s, dummyErr)

	fun impossible m = ErrorMsg.impossible ("CoreAccess: " ^ m)
    in
	fun getVar' err x =
	    (case getCore x of
		 VarCon.VAL r => r
	       | _ => impossible "getVar")
	    handle NoCore => err ()

	fun getVar x = getVar' (fn () => impossible "getVar") x

	fun getCon' err x =
	    (case getCore x of
		 VarCon.CON c => c
	       | _ => err ())
	    handle NoCore => err ()

	fun getCon x = getCon' (fn () => impossible "getCon") x

	fun getExn x = getCon' (fn () => VarCon.bogusEXN) x
    end
end
