(*
 * A tool for running arbitrary shell commands from CM.
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure ShellTool = struct
    local
	open Tools

	val tool = "Shell-Command"
	val class = "shell"
	val kw_class = "class"
	val kw_options = "options"
	val kw_source = "source"
	val kw_target = "target"
	val kwl = [kw_class, kw_options, kw_source, kw_target]

	fun error msg = raise ToolError { tool = tool, msg = msg }

	fun rule { spec, context, mkNativePath } = let
	    val (str, pathmaker, _, too) = spec
	    val (sstr, tstr, tclass, topts, cmdline) =
		case too of
		    NONE => error "missing options"
		  | SOME ol => let
			val { matches, restoptions } = parseOptions
			    { tool = tool, keywords = kwl, options = ol }
			val tclass = matches kw_class
			val topts = Option.map tokenize (matches kw_options)
			fun return (sstr, tstr) = let
			    fun subst "%s" = sstr
			      | subst "%t" = tstr
			      | subst "%%s" = "%s"
			      | subst "%%t" = "%t"
			      | subst s = s
			    fun ad (x, l) = " " :: subst x :: l
			    val cmdline =
				case restoptions of
				    [] => error "no command line specified"
				  | h :: t => concat (subst h :: foldr ad [] t)
			in
			    (sstr, tstr, tclass, topts, cmdline)
			end
		    in
			case (matches kw_source, matches kw_target) of
			    (NONE, NONE) => error
			      "either `source=' or `target=' must be specified"
			  | (SOME src, NONE) => return (src, str)
			  | (NONE, SOME tgt) => return (str, tgt)
			  | (SOME _, SOME _) => error
			 "only one of `source=' and `target=' can be specified"
		    end
	    val sname = nativeSpec (pathmaker sstr)
	    val tname = nativeSpec (pathmaker tstr)
	    val partial_expansion =
		({ smlfiles = [], cmfiles = [] },
		 [(tname, mkNativePath, tclass, topts)])
	    fun runcmd () =
		(vsay ["[", cmdline, "]\n"];
		 if OS.Process.system cmdline = OS.Process.success then ()
		 else raise ToolError { tool = tool, msg = cmdline })
	    fun rulefn () =
		(if outdated tool ([tname], sname) then runcmd ()
		 else ();
		 partial_expansion)
	in
	    context rulefn
	end
    in
        val _ = Tools.registerClass (class, rule)
    end
end
