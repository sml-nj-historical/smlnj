(*
 * CM parameters that are configurable via shell-environment variables.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure StdConfig = struct
    local
	val bool = EnvConfig.new Bool.fromString
	val int = EnvConfig.new Int.fromString
	val string = EnvConfig.new SOME
	val stringoptthunk = EnvConfig.new (fn s => SOME (fn () => SOME s))
    in
	val verbose = bool ("VERBOSE", true)
	val debug = bool ("DEBUG", false)
	val keep_going = bool ("KEEP_GOING", false)
	val pathcfgspec = string ("PATHCONFIG", "/usr/lib/smlnj-pathconfig")
	val parse_caching = int ("PARSE_CACHING", 100)
	val local_pathconfig =
	    stringoptthunk ("LOCAL_PATHCONFIG",
			    fn () =>
			      Option.map (fn h => OS.Path.concat
					   (h, ".smlnj-pathconfig"))
			                 (OS.Process.getEnv "HOME"))
	val warn_obsolete = bool ("WARN_OBSOLETE", true)
	val conserve_memory = bool ("CONSERVE_MEMORY", false)
	val generate_index = bool ("GENERATE_INDEX", false)
    end
end
