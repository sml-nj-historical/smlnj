(*
 * A tool for running "make" from CM.
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure MakeTool = struct
    local
	open Tools

	val tool = "Make-Command"	(* the name of this tool *)
	val class = "make"		(* the name of the class *)
	val stdCmdPath = "make"		(* the shell command to invoke it *)
	val kw_class = "class"
	val kw_options = "options"

	fun err m = raise ToolError { tool = tool, msg = m }

	fun rule { spec, context, mkNativePath } = let
	    val (str, pathmaker, _, too) = spec
	    val (tclass, topts, mopts) =
		case too of
		    NONE => (NONE, NONE, [])
		  | SOME options => let
			val { matches, restoptions } =
			    parseOptions
				{ tool = tool,
				  keywords = [kw_class, kw_options],
				  options = options }
		    in
			(case matches kw_class of
			     SOME [STRING { name, ... }] => SOME name
			   | NONE => NONE
			   | _ => err "invalid class specification",
			 matches kw_options,
			 restoptions)
		    end
	    val tname = nativeSpec (pathmaker str)
	    val partial_expansion =
		({ smlfiles = [], cmfiles = [] },
		 [(tname, mkNativePath, tclass, topts)])
	    fun runcmd () = let
		val cmdname = mkCmdName stdCmdPath
		val cmd = concat (cmdname :: foldr (fn (x, l) => " " :: x :: l)
				                   [" ", tname] mopts)
	    in
		vsay ["[", cmd, "]\n"];
		if OS.Process.system cmd = OS.Process.success then ()
		else err cmd
	    end
	    fun rulefn () = (runcmd (); partial_expansion)
	in
	    context rulefn
	end
    in
        val _ = registerClass (class, rule);
    end
end
