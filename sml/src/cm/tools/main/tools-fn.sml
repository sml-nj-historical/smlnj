(*
 * Functor implementing the public interface to CM's tools mechanism.
 *   (This functor must be instantiated after the rest of CM is
 *    already in place because it needs to uses load_plugin.)
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor ToolsFn (val load_plugin : string -> bool
		 val load_plugin' : SrcPath.file -> bool
		 val penv: SrcPath.env) : TOOLS = struct

    open PrivateTools
    val defaultClassOf = defaultClassOf load_plugin

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
	fun rule { spec, context, mkNativePath } = let
	    val { name, mkpath, opts = oto, derived, ... } : spec = spec
	    val opts = getOpt (oto, dflopts)
	    val sol = let		(* only use STRING options for %o *)
		fun so (SUBOPTS _) = NONE
		  | so (STRING { name, mkpath }) =
		    SOME (nativeSpec (srcpath (mkpath name)))
	    in
		List.mapPartial so opts
	    end
	    val p = srcpath (mkpath name)
	    val nativename = nativeSpec p
	    val tfiles = extend extensionStyle (nativename, oto)
	    val partial_expansion =
		({ smlfiles = [], cmfiles = [],
		   sources = [(p, { class = class, derived = derived })] },
		 map (fn (f, co, too) => { name = f,
					   mkpath = mkNativePath,
					   class = co,
					   opts = too,
					   derived = true })
		     tfiles)
	    fun runcmd () = let
		val cmdname = mkCmdName cmdStdPath
		fun fill ([], sl) = concat (rev sl)
		  | fill (#"%" :: #"%" :: t, sl) = fill (t, "%" :: sl)
		  | fill (#"%" :: #"c" :: t, sl) = fill (t, cmdname :: sl)
		  | fill (#"%" :: #"s" :: t, sl) = fill (t, nativename :: sl)
		  | fill (#"%" :: t, sl0) = let
			val o0 = Char.ord #"0"
			fun select (0, cl, sl, ol, sel) =
			    fill (cl, foldl (fn (x, l) => sel x :: " " :: l)
					    sl0 ol)
			  | select (n, cl, sl, ol, sel) =
			    (fill (cl, sel (List.nth (ol, n-1)) :: sl0)
			     handle General.Subscript => fill (cl, sl))
			fun loop (n, [], sl) = fill ([], sl)
			  | loop (n, t as (c :: t'), sl) =
			    if c >= #"0" andalso c <= #"9" then
				loop (n * 10 + Char.ord c - o0,
				      t', String.str c :: sl)
			    else let
				val sl = String.str c :: sl
			    in
				case c of
				    #"o" => select (n, t', sl, sol, fn x => x)
				  | #"t" => select (n, t', sl, tfiles, #1)
				  | _ => fill (t', sl)
			    end
		    in
			loop (0, t, "%" :: sl0)
		    end
		  | fill (c :: t, sl) = fill (t, String.str c :: sl)
		val cmd = fill (String.explode template, [])
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
	fun toolrule { spec, context, mkNativePath } = let
	    val { name, mkpath, opts, ... } : spec = spec
	    fun err m = raise ToolError { tool = toolclass, msg = m }
	    val p = srcpath (mkpath name)
	in
	    case opts of
		NONE => if withPlugin p (fn () => load_plugin' p) then
			    empty_expansion
			else err "tool registration failed"
	      | SOME _ => err "no tool options are recognized"
	end
	fun suffixrule { spec, context, mkNativePath } = let
	    val { name = s, opts, ... } : spec = spec
	    fun err m = raise ToolError { tool = suffixclass, msg = m }
	    fun reg c =
		(registerClassifier (stdSfxClassifier { sfx = s, class = c });
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
end
