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
    in
	val verbose = bool ("VERBOSE", true)
	val debug = bool ("DEBUG", false)
	val keep_going = bool ("KEEP_GOING", false)

	val parse_caching = int ("PARSE_CACHING", 100)
    end
end
