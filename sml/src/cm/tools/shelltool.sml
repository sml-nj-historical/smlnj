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

	fun err msg = raise ToolError { tool = tool, msg = msg }
	fun badspec kw = err (concat ["bad specification for keyword `",
				      kw, "'"])

	fun rule { spec, context, mkNativePath } = let
	    val (str, pathmaker, _, too) = spec
	    val specname = nativeSpec (pathmaker str)
	    val (sname, tname, tclass, topts, cmdline) =
		case too of
		    NONE => err "missing options"
		  | SOME ol => let
			val { matches, restoptions } =
			    parseOptions
			       { tool = tool, keywords = kwl, options = ol }
			fun fmatch kw =
			    case matches kw of
				NONE => NONE
			      | SOME [STRING { name, mkpath }] =>
				  SOME (nativeSpec (mkpath name))
			      | _ => badspec kw
			val tclass =
			    case matches kw_class of
				NONE => NONE
			      | SOME [STRING { name, ... }] => SOME name
			      | _ => badspec kw_class
			val topts = matches kw_options
			fun return (sname, tname) = let
			    fun subst "%s" = sname
			      | subst "%t" = tname
			      | subst "" = ""
			      | subst s = if String.sub (s, 0) = #"%" then
					      String.extract (s, 1, NONE)
					  else s
			    fun ad (x, l) = " " :: subst x :: l
			    val cmdline =
				case restoptions of
				    [] => err "no command line specified"
				  | h :: t => concat (subst h :: foldr ad [] t)
			in
			    (sname, tname, tclass, topts, cmdline)
			end
		    in
			case (fmatch kw_source, fmatch kw_target) of
			    (NONE, NONE) => err
			      "either `source=' or `target=' must be specified"
			  | (SOME src, NONE) => return (src, specname)
			  | (NONE, SOME tgt) => return (specname, tgt)
			  | (SOME _, SOME _) => err
			 "only one of `source=' and `target=' can be specified"
		    end
	    val partial_expansion =
		({ smlfiles = [], cmfiles = [] },
		 [(tname, mkNativePath, tclass, topts)])
	    fun runcmd () =
		(vsay ["[", cmdline, "]\n"];
		 if OS.Process.system cmdline = OS.Process.success then ()
		 else err cmdline)
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
