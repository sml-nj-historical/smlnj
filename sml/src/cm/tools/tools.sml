(*
 * Target expansion and CM tools.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CORETOOLS = sig

    (* We don't make classes abstract.  It doesn't look like there
     * would be much point to it. *)
    type class = string

    (* We keep source paths abstract. Tool writers should not mess
     * with their internals.
     * The function that makes a srcpath from a string is passed as
     * part of the input specification (type "spec").  Which function
     * is originally being passed depends on which syntax was used for
     * this member in its .cm-file.  Most tools will want to work on
     * native pathname syntax (function "outdated" -- see below -- depends
     * on native syntax!).  In these cases the tool should first convert
     * the name to a srcpath and then get the native string back by 
     * applying "nativeSpec". *)
    type srcpath

    val nativeSpec : srcpath -> string

    exception ToolError of { tool: string, msg: string }

    type pathmaker = string -> srcpath

    (* A member specification consists of the actual string, an optional
     * class name, and a function to convert a string to its corresponding
     * srcpath. *)
    type spec = string * pathmaker * class option

    (* The goal of applying tools to members is to obtain an "expansion",
     * i.e., a list of ML-files and a list of .cm-files. *)
    type expansion = { smlfiles: (srcpath * Sharing.request) list,
		       cmfiles: srcpath list }

    (* A partial expansion is an expansion with a list of things yet to be
     * expanded... *)
    type partial_expansion = expansion * spec list

    (* A rule takes a spec and a rulecontext where the name contained
     * in the spec -- if relative -- is considered relative to the directory
     * of the corresponding description file.  In general,
     * when coding a rule one would write a rule function and pass it to
     * the context, which will temporarily change the current working
     * directory to the one that holds the description file ("the context").
     * If this is not necessary for the rule to work correctly, then
     * one can simply ignore the context (this saves system call overhead
     * during dependency analysis).
     * If the rule yields a genuine partial expansion (where the resulting
     * spec list is not empty), then it must pass the proper "path maker"
     * along with each new name.  For most cases this will be the given
     * "native path maker" because most rules work on native path names.
     * Some rules, however, might want to use the same convention for
     * derived specs that was used for the original spec. *)
    type rulefn = unit -> partial_expansion
    type rulecontext = rulefn -> partial_expansion
    type rule =
	{ spec: spec, mkNativePath: pathmaker, context: rulecontext } ->
	partial_expansion

    (* install a class *)
    val registerClass : class * rule -> unit

    (* classifiers are used when the class is not given explicitly *)
    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of string -> class option

    (* make a classifier which looks for a specific file name suffix *)
    val stdSfxClassifier : { sfx: string , class: class } -> classifier

    (* two standard ways of dealing with filename extensions... *)
    datatype extensionStyle =
	EXTEND of (string * class option) list
      | REPLACE of string list * (string * class option) list

    (* perform filename extension *)
    val extend : extensionStyle -> string -> (string * class option) list

    (* check for outdated files; the pathname strings must be in
     * native syntax! *)
    val outdated : string -> string list * string -> bool

    (* install a classifier *)
    val registerClassifier : classifier -> unit
end

signature PRIVATETOOLS = sig
    include CORETOOLS where type srcpath = SrcPath.t

    val expand : { error: string -> unit,
		   spec: spec,
		   context: SrcPath.context,
		   load_plugin: string -> bool }
	-> expansion

    val defaultClassOf : (string -> bool) -> string -> class option
end

signature TOOLS = sig
    include CORETOOLS

    (* Register a "standard" tool based on some shell command. *)
    val registerStdShellCmdTool : { tool: string,
				    class: string,
				    suffixes: string list,
				    cmdStdPath: string,
				    extensionStyle: extensionStyle } -> unit

    (* query default class *)
    val defaultClassOf : string -> class option
end

structure PrivateTools :> PRIVATETOOLS = struct

    type class = string

    type srcpath = SrcPath.t

    val nativeSpec = SrcPath.specOf

    exception ToolError of { tool: string, msg: string }

    type pathmaker = string -> srcpath

    type spec = string * pathmaker * class option

    type expansion = { smlfiles: (srcpath * Sharing.request) list,
		       cmfiles: srcpath list }

    type partial_expansion = expansion * spec list

    type rulefn = unit -> partial_expansion
    type rulecontext = rulefn -> partial_expansion
    type rule =
	{ spec: spec, mkNativePath: pathmaker, context: rulecontext } ->
	partial_expansion

    val classes : rule StringMap.map ref = ref StringMap.empty

    fun registerClass (class, rule) =
	classes := StringMap.insert (!classes, class, rule)

    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of string -> class option

    fun stdSfxClassifier { sfx, class } =
	SFX_CLASSIFIER (fn e => if sfx = e then SOME class else NONE)

    datatype extensionStyle =
	EXTEND of (string * class option) list
      | REPLACE of string list * (string * class option) list

    fun extend (EXTEND l) f = map (fn (s, co) => (concat [f, ".", s], co)) l
      | extend (REPLACE (ol, nl)) f = let
	    val { base, ext } = OS.Path.splitBaseExt f
	    fun join b (e, co) =
		(OS.Path.joinBaseExt { base = b, ext = SOME e }, co)
	    fun gen b = map (join b) nl
	    fun sameExt (e1: string) (e2: string) = e1 = e2
	in
	    case ext of
		NONE => gen base
	      | SOME e =>
		    if List.exists (sameExt e) ol then gen base else gen f
	end

    fun outdated tool (l, f) = let
	val (ftime, fexists) =
	    (OS.FileSys.modTime f, true)
	    handle _ => (Time.zeroTime, false)
	fun olderThan t f = Time.< (OS.FileSys.modTime f, t)
    in
	(List.exists (olderThan ftime) l)
	handle _ => if fexists then true
		    else raise ToolError { tool = tool,
					   msg = "cannot access " ^ f }
    end

    val sfx_classifiers : (string -> class option) list ref = ref []
    val gen_classifiers : (string -> class option) list ref = ref []

    local
	fun add (x, r) = r := x :: (!r)
    in
	fun registerClassifier (SFX_CLASSIFIER c) = add (c, sfx_classifiers)
	  | registerClassifier (GEN_CLASSIFIER c) = add (c, gen_classifiers)
    end

    (* query default class *)
    fun defaultClassOf load_plugin p = let
	fun gen_loop [] = NONE
	  | gen_loop (h :: t) =
	    (case h p of
		 NONE => gen_loop t
	       | SOME c => SOME c)

	fun sfx_loop e = let
	    fun loop [] = gen_loop (!gen_classifiers)
	      | loop (h :: t) =
		(case h e of
		     NONE => loop t
		   | SOME c => SOME c)
	in
	    loop (!sfx_classifiers)
	end
    in
	case OS.Path.ext p of
	    SOME e =>
		(case sfx_loop e of
		     SOME c => SOME c
		   | NONE => let
			 val plugin = OS.Path.joinBaseExt { base = e ^ "-ext",
							    ext = SOME "cm" }
		     in
			 if load_plugin plugin then sfx_loop e else NONE
		     end)
	  | NONE => gen_loop (!gen_classifiers)
    end

    fun smlrule srq { spec = (name, mkpath, _), context, mkNativePath } =
	({ smlfiles = [(mkpath name, srq)], cmfiles = [] }, [])
    fun cmrule { spec = (name, mkpath, _), context, mkNativePath } =
	({ smlfiles = [], cmfiles = [mkpath name] }, [])

    fun expand { error, spec, context, load_plugin } = let
	fun mkNativePath s = SrcPath.native { context = context, spec = s }
	fun class2rule class =
	    case StringMap.find (!classes, class) of
		SOME rule => rule
	      | NONE => let
		    val plugin = OS.Path.joinBaseExt { base = class ^ "-tool",
						       ext = SOME "cm" }
		    fun complain () =
			(error (concat ["unknown class \"", class, "\""]);
			 smlrule Sharing.DONTCARE)
		in
		    if load_plugin plugin then
			case StringMap.find (!classes, class) of
			    SOME rule => rule
			  | NONE => complain ()
		    else complain ()
		end

	fun expand1 (spec as (name, _, co)) = let
	    val rule =
		case co of
		    SOME c0 => class2rule (String.map Char.toLower c0)
		  | NONE =>
			(case defaultClassOf load_plugin name of
			     SOME c => class2rule c
			   | NONE => smlrule Sharing.DONTCARE)
	    fun rcontext rf = let
		val dir = SrcPath.contextName context
		val cwd = OS.FileSys.getDir ()
	    in
		SafeIO.perform { openIt = fn () => OS.FileSys.chDir dir,
				 closeIt = fn () => OS.FileSys.chDir cwd,
				 work = rf,
				 cleanup = fn _ => () }
		handle ToolError { tool, msg } =>
		    (error (concat ["tool \"", tool, "\" failed: ", msg]);
		     ({ smlfiles = [], cmfiles = [] }, []))
	    end
	in
	    rule { spec = spec, context = rcontext,
		   mkNativePath = mkNativePath }
	end
	fun loop (expansion, []) = expansion
	  | loop ({ smlfiles, cmfiles }, item :: items) = let
		val ({ smlfiles = sfl, cmfiles = cfl }, il) = expand1 item
	    in
		loop ({ smlfiles = smlfiles @ sfl, cmfiles = cmfiles @ cfl},
		      il @ items)
	    end
    in
	loop ({ smlfiles = [], cmfiles = [] }, [spec])
    end

    local
	fun sfx (s, c) =
	    registerClassifier (stdSfxClassifier { sfx = s, class = c })
    in
	val _ = registerClass ("sml", smlrule Sharing.DONTCARE)
	val _ = registerClass ("shared", smlrule Sharing.SHARED)
	val _ = registerClass ("private", smlrule Sharing.PRIVATE)
	val _ = registerClass ("cm", cmrule)

	val _ = sfx ("sml", "sml")
	val _ = sfx ("sig", "sml")
	val _ = sfx ("cm", "cm")
    end
end

functor ToolsFn (val load_plugin : string -> bool
		 val mkStdSrcPath : string -> SrcPath.t) : TOOLS = struct
    open PrivateTools
    val defaultClassOf = defaultClassOf load_plugin

    fun registerStdShellCmdTool args = let
	val { tool, class, suffixes, cmdStdPath, extensionStyle } = args
	fun mkCmdName () = let
	    (* It is not enough to turn the string into a SrcPath.t
	     * once.  This is because if there was no anchor in the
	     * beginning, later additions of an anchor will go unnoticed.
	     * This is different from how other files (ML source files)
	     * behave: They, once the are found to be unanchored, should
	     * never become anchored later (although an existing anchor
	     * is allowed to change). *)
	    val p = mkStdSrcPath cmdStdPath
	    val n = SrcPath.osstring p
	in
	    (* If the resulting path is not absolute, then it cannot have
	     * been anchored (configured). In this case we just use the
	     * given string as-is. *)
	    if OS.Path.isAbsolute n then n else cmdStdPath
	end
	fun rule { spec = (name, mkpath, _), context, mkNativePath } = let
	    val nativename = nativeSpec (mkpath name)
	    val targetfiles = extend extensionStyle nativename
	    val partial_expansion =
		({ smlfiles = [], cmfiles = [] },
		 map (fn (f, co) => (f, mkNativePath, co)) targetfiles)
	    fun runcmd () = let
		val cmd = concat [mkCmdName (), " ", nativename]
		val _ = Say.vsay ["[", cmd, "]\n"]
	    in
		if OS.Process.system cmd = OS.Process.success then ()
		else raise ToolError { tool = tool, msg = cmd }
	    end
	    fun rulefn () =
		(if outdated tool (map #1 targetfiles, nativename) then
		     runcmd ()
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
end
