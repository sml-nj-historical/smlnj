(* compinfo.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure CompInfo = struct

    type compInfo = { mkStamp: unit -> Stamps.stamp,
                      mkLvar: Symbol.symbol option -> Access.lvar,
                      anyErrors: bool ref,
                      error: ErrorMsg.errorFn,
                      errorMatch: SourceMap.region -> string,
                      transform: Absyn.dec -> Absyn.dec,
                      sourceName : string }

    fun mkCompInfo { source, transform, mkMkStamp } = let
	val { error, errorMatch, anyErrors } = ErrorMsg.errors source
	val _ = LambdaVar.clear ()
	val g = mkMkStamp ()
	fun mkLvar NONE = LambdaVar.mkLvar ()
	  | mkLvar (SOME sym) = LambdaVar.namedLvar sym
    in
	{ mkStamp = fn () => Stamps.fresh g,
	  mkLvar = mkLvar,
	  anyErrors = anyErrors,
	  error = error,
	  errorMatch = errorMatch,
	  transform = transform,
	  sourceName = #fileOpened source } : compInfo
    end

    fun anyErrors (ci : compInfo) = ! (#anyErrors ci)

end (* structure CompBasic *)
