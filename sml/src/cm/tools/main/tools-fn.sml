(*
 * Functor implementing the public interface to CM's tools mechanism.
 *   (This functor must be instantiated after the rest of CM is
 *    already in place because it uses load_plugin.)
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor ToolsFn (val load_plugin' : SrcPath.file -> bool
		 val penv: SrcPath.env) : TOOLS = struct

    open PrivateTools

    val say = Say.say
    val vsay = Say.vsay

    fun mkCmdName cmdStdPath =
	(* The result of this function should not be cached. Otherwise
	 * a later addition or change of an anchor will go unnoticed. *)
	case SrcPath.get_anchor (penv, cmdStdPath) of
	    NONE => cmdStdPath
	  | SOME p => OS.Path.joinDirFile { dir = p, file = cmdStdPath }

    fun registerStdShellCmdTool args = let
	val { tool, class, suffixes, cmdStdPath,
	      extensionStyle, template, dflopts } = args
	val template = getOpt (template, "%c %s")
	fun err m = raise ToolError { tool = tool, msg = m }
	fun rule { spec, context, native2pathmaker, defaultClassOf, sysinfo } = let
	    val { name, mkpath, opts = oto, derived, ... } : spec = spec
	    val opts = getOpt (oto, dflopts)
	    val sol = let		(* only use STRING options for %o *)
		fun so (SUBOPTS _) = NONE
		  | so (STRING s) = SOME (nativeSpec (srcpath (#mkpath s ())))
	    in
		List.mapPartial so opts
	    end
	    val p = srcpath (mkpath ())
	    val nativename = nativeSpec p
	    val tfiles = extend extensionStyle (nativename, oto)
	    val partial_expansion =
		({ smlfiles = [], cmfiles = [],
		   sources = [(p, { class = class, derived = derived })] },
		 map (fn (f, co, too) => { name = f,
					   mkpath = native2pathmaker f,
					   class = co,
					   opts = too,
					   derived = true })
		     tfiles)
	    fun runcmd () = let
		val cmdname = mkCmdName (cmdStdPath ())
		val cmd =
		    Subst.substitute
			[{ prefix = "%",
			   substitutions = [Subst.subfor "%c" cmdname,
					    Subst.subfor "%s" nativename,
					    Subst.subfor "%%" "%",
					    Subst.subnsel (1, #"o", fn x => x, " ") sol,
					    Subst.subnsel (1, #"t", #1, " ") tfiles] }]
			template
	    in
		Say.vsay ["[", cmd, "]\n"];
		if OS.Process.system cmd = OS.Process.success then ()
		else err cmd
	    end
	    fun rulefn () =
		(if outdated tool (map #1 tfiles, nativename) then runcmd ()
		 else ();
		 partial_expansion)
	in
	    context rulefn
	end
	fun sfx s =
	    registerClassifier (stdSfxClassifier { sfx = s, class = class })
    in
	registerClass (class, rule);
	app sfx suffixes
    end

    local
	val toolclass = "tool"
	val suffixclass = "suffix"
	val empty_expansion =
	    ({ cmfiles = [], smlfiles = [], sources = [] }, [])
	fun toolrule { spec, context, native2pathmaker, defaultClassOf, sysinfo } =
	    let val { name, mkpath, opts, ... } : spec = spec
		fun err m = raise ToolError { tool = toolclass, msg = m }
		val p = srcpath (mkpath ())
	    in
		case opts of
		    NONE => if withPlugin p (fn () => load_plugin' p) then
				empty_expansion
			    else err "tool registration failed"
		  | SOME _ => err "no tool options are recognized"
	    end
	fun suffixrule { spec, context, native2pathmaker, defaultClassOf, sysinfo } =
	    let val { name = s, opts, ... } : spec = spec
		fun err m = raise ToolError { tool = suffixclass, msg = m }
		fun reg c =
		    (registerClassifier (stdSfxClassifier { sfx = s,
							    class = c });
		     empty_expansion)
	    in
		case opts of
		    SOME [STRING c] => reg (#name c)
		  | SOME [SUBOPTS { name = "class", opts = [STRING c] }] =>
		    reg (#name c)
		  | _ => err "invalid options"
	    end
    in
        val _ = registerClass (toolclass, toolrule)
	val _ = registerClass (suffixclass, suffixrule)
    end

    fun boolcontrol (name, doc, default) =
	StdConfig.new (ControlUtil.Cvt.bool, name, doc, default)
end
