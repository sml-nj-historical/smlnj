(* coreacc.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure CoreAccess : sig
    val getVar : StaticEnv.staticEnv -> string list -> VarCon.var
    val getCon : StaticEnv.staticEnv -> string list -> VarCon.datacon
    val getVar' : (unit -> VarCon.var) ->
		  StaticEnv.staticEnv -> string list -> VarCon.var
    val getCon' : (unit -> VarCon.datacon) ->
		  StaticEnv.staticEnv -> string list -> VarCon.datacon

    (* like getCon, but returns a bogus exn instead of failing *)
    val getExn : StaticEnv.staticEnv -> string list -> VarCon.datacon
end = struct

    local
	fun impossible m = ErrorMsg.impossible ("CoreAccess: " ^ m)

	exception NoCore
	fun dummyErr _ _ _ = raise NoCore
	fun mkpath [] = impossible "mkpath"
	  | mkpath [x] = [Symbol.varSymbol x]
	  | mkpath (x :: xs) = Symbol.strSymbol x :: mkpath xs
	fun path xs = SymPath.SPATH (CoreSym.coreSym :: mkpath xs)
	fun getCore env xs = Lookup.lookVal (env, path xs, dummyErr)
    in
	fun getVar' err env xs =
	    (case getCore env xs of
		 VarCon.VAL r => r
	       | _ => impossible "getVar")
	    handle NoCore => err ()

	fun getVar env xs = getVar' (fn () => impossible "getVar") env xs

	fun getCon' err env xs =
	    (case getCore env xs of
		 VarCon.CON c => c
	       | _ => err ())
	    handle NoCore => err ()

	fun getCon env xs = getCon' (fn () => impossible "getCon") env xs

	fun getExn env xs = getCon' (fn () => VarCon.bogusEXN) env xs
    end
end
