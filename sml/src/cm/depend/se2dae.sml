(*
 * Convert a given static env to a "dependency-analysis env".
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature STATENV2DAENV = sig
    val cvt :
	GenericVC.Environment.staticEnv -> DAEnv.env * (unit -> SymbolSet.set)
end

structure Statenv2DAEnv :> STATENV2DAENV = struct

    structure BE = GenericVC.BareEnvironment

    fun cvt se = let
	fun l2s l = let
	    fun addModule (sy, set) =
		case Symbol.nameSpace sy of
		    (Symbol.STRspace | Symbol.SIGspace |
		     Symbol.FCTspace | Symbol.FSIGspace) =>
		    SymbolSet.add (set, sy)
		   | _ => set
	in
	    foldl addModule SymbolSet.empty l
	end
	fun cvt_fctenv look = DAEnv.FCTENV (cvt_result o look)
	and cvt_result (BE.CM_ENV { look, ... }) = SOME (cvt_fctenv look)
	  | cvt_result BE.CM_NONE = NONE
	val sb = GenericVC.CoerceEnv.es2bs se
	val dae = cvt_fctenv (BE.cmEnvOfModule sb)
	fun mkDomain () = l2s (BE.catalogEnv sb)
    in
	(dae, mkDomain)
    end
end
