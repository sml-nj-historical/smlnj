(*
 * CM parameters that are configurable via shell-environment variables.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure EnvConfig = struct
    local
	fun cfg cvt reg session fallback = let
	    val default = session ^ "_DEFAULT"
	    val getEnv = Option.join o (Option.map cvt) o OS.Process.getEnv
	    val r = ref (getOpt (getEnv default, fallback))
	    fun getterSetter arg =
		!r before (case arg of SOME new => r := new | NONE => ())
	    val reg = fn () => (reg (); ignore (getterSetter (getEnv session)))
	in
	    (getterSetter, reg)
	end
	val bool = Bool.fromString
	fun string s = SOME s
	val int = Int.fromString
    in
	val r = fn () => ()

	val (verbose, r) = cfg bool r "CM_VERBOSE" true
	val (debug, r) = cfg bool r "CM_DEBUG" false
	val (keep_going, r) = cfg bool r "CM_KEEP_GOING" false
	val (show_exports, r) = cfg bool r "CM_SHOW_EXPORTS" false

	val (lex, r) = cfg string r "CM_LEX" "ml-lex"
	val (yacc, r) = cfg string r "CM_YACC" "ml-yacc"
	val (burg, r) = cfg string r "CM_BURG" "ml-burg"
	val (rcsco, r) = cfg string r "CM_RCSCO" "co -q"

	val (parse_caching, r) = cfg int r "CM_PARSE_CACHING" 100

	val register_all = r
    end
end
