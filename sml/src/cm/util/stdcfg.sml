(*
 * CM parameters that are configurable via shell-environment variables.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure StdConfig = struct
    local
	val priority = [10, 2]
	val obscurity = 2
	val prefix = "cm"

	val registry = ControlRegistry.new
			   { help = "Compilation Manager (CM)" }

	val _ = BasicControl.nest (prefix, registry, priority)

	val bool_cvt = ControlUtil.Cvt.bool
	val int_cvt = ControlUtil.Cvt.int

	val sot_cvt =
	    { tyName = "string",	(* string option thunk *)
	      fromString = fn s => SOME (fn () => SOME s),
	      toString = fn th => (case th () of
				       SOME s => s
				     | NONE => "(not set)") }

	val string_cvt = ControlUtil.Cvt.string

	val nextpri = ref 0

	fun new (c, n, h, d) = let
	    val r = ref d
	    val p = !nextpri
	    val ctl = Controls.control { name = n,
					 pri = [p],
					 obscurity = obscurity,
					 help = h,
					 ctl = r }
	in
	    nextpri := p + 1;
	    ControlRegistry.register
		registry
		{ ctl = Controls.stringControl c ctl,
		  envName = SOME (ControlUtil.EnvName.toUpper "CM_" n) };
	    { set = fn x => r := x,
	      get = fn () => !r }
	end
    in
        val verbose = new (bool_cvt, "verbose", "CM chattiness", true)
	val debug = new (bool_cvt, "debug", "CM debug mode", false)
	val keep_going = new (bool_cvt, "keep-going",
			      "whether CM presses on in face of errors",
			      false)
	val pathcfgspec = new (string_cvt, "pathconfig",
			       "global path configuration file",
			       "/usr/lib/smlnj-pathconfig")
	val parse_caching =
	    new (int_cvt, "parse-caching", "limit on parse trees cached", 100)
	val local_pathconfig =
	    new (sot_cvt, "local-pathconfig", "local path configuration file",
		 fn () => Option.map (fn h => OS.Path.concat
						  (h, ".smlnj-pathconfig"))
				     (OS.Process.getEnv "HOME"))
	val warn_obsolete = new (bool_cvt, "warn-obsolete",
				 "whether CM accepts old-style syntax",
				 true)
	val conserve_memory =
	    new (bool_cvt, "conserve-memory", "CM memory stinginess", false)
	val generate_index = new (bool_cvt, "generate-index",
				  "whether CM generates library indices",
				  false)
    end
end
