(*
 * CM parameters that are configurable via shell-environment variables.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure StdConfig = struct
    local
	structure C = Controls

	val m = C.registry { name = "Compilation Manager (CM)",
			     priority = [10, 2],
			     obscurity = 2,
			     prefix = "cm-",
			     default_suffix = SOME "-default",
			     mk_ename = NONE }

	fun new typespec = let
	    val r = C.group m typespec
	in
	    fn individual => C.ref2var (C.new r individual)
	end

	val bool = new C.bool
	val int = new C.int
	val string = new C.string
	val stringoptthunk = new { tname = "string",
				   fromString = fn s => SOME (fn () => SOME s),
				   toString = fn th =>
						 (case th () of
						      SOME s => s
						    | NONE => "(not set)") }
    in
        val verbose =
	    bool { stem = "verbose", fallback = true,
		   descr = "CM chattiness" }
	val debug =
	    bool { stem = "debug", fallback = false,
		   descr = "CM debug mode" }
	val keep_going =
	    bool { stem = "keep-going", fallback = false,
		   descr = "whether CM presses on in face of errors" }
	val pathcfgspec =
	    string { stem = "pathconfig",
		     fallback = "/usr/lib/smlnj-pathconfig",
		     descr = "global path configuration file" }
	val parse_caching =
	    int { stem = "parse-caching", fallback = 100,
		  descr = "limit on parse trees cached" }
	val local_pathconfig =
	    stringoptthunk { stem = "local-pathconfig",
			     fallback = fn () =>
				Option.map (fn h => OS.Path.concat
						(h, ".smlnj-pathconfig"))
					   (OS.Process.getEnv "HOME"),
			     descr = "local path configuration file" }
	val warn_obsolete =
	    bool { stem = "warn-obsolete", fallback = true,
		   descr = "whether CM accepts old-style syntax" }
	val conserve_memory =
	    bool { stem = "conserve-memory", fallback =false,
		   descr = "CM memory stinginess" }
	val generate_index =
	    bool { stem = "generate-index", fallback = false,
		   descr = "whether CM generates library indices" }
    end
end
