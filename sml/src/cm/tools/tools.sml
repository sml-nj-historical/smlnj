(*
 * Target expansion and CM tools.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature TOOLS = sig

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

    (* A rule takes a spec and a rulecontext where the name name contained
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
	EXTEND of string list
      | REPLACE of string list * string list

    type cmdController = { get: unit -> string, set: string -> unit }

    val newCmdController : string * string -> cmdController

    val registerStdShellCmdTool : { tool: string,
				    class: string,
				    suffixes: string list,
				    command: cmdController,
				    extensionStyle: extensionStyle,
				    sml: bool } -> unit

    (* perform filename extension *)
    val extend : extensionStyle -> string -> string list

    (* check for outdated files; the pathname strings must be in
     * native syntax! *)
    val outdated : string -> string list * string -> bool

    (* install a classifier *)
    val registerClassifier : classifier -> unit

    (* query default class *)
    val defaultClassOf : string -> class option
end

signature PRIVATETOOLS = sig
    include TOOLS where type srcpath = SrcPath.t
    val expand : { error: string -> unit,
		   spec: spec,
		   context: SrcPath.context }
	-> expansion
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
	EXTEND of string list
      | REPLACE of string list * string list

    type cmdController = { get: unit -> string, set: string -> unit }

    fun newCmdController sp = EnvConfig.new SOME sp

    fun extend (EXTEND l) f = map (fn s => concat [f, ".", s]) l
      | extend (REPLACE (ol, nl)) f = let
	    val { base, ext } = OS.Path.splitBaseExt f
	    fun join b e = OS.Path.joinBaseExt { base = b, ext = SOME e }
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

    fun registerStdShellCmdTool args = let
	val { tool, class, suffixes, command, extensionStyle, sml } = args
	fun rule { spec = (name, mkpath, _), context, mkNativePath } = let
	    val nativename = nativeSpec (mkpath name)
	    val targetfiles = extend extensionStyle nativename
	    val partial_expansion =
		if sml then
		    ({ smlfiles =
		        map (fn f => (mkNativePath f, Sharing.DONTCARE))
			    targetfiles,
		       cmfiles = [] },
		     [])
		else ({ smlfiles = [], cmfiles = [] },
		      map (fn f => (f, mkNativePath, NONE)) targetfiles)
	    fun runcmd () = let
		val cmd =
		    concat [#get (command: cmdController) (), " ", nativename]
		val _ = Say.vsay ["[", cmd, "]\n"]
	    in
		if OS.Process.system cmd = OS.Process.success then ()
		else raise ToolError { tool = tool, msg = cmd }
	    end
	    fun rulefn () =
		(if outdated tool (targetfiles, nativename) then runcmd ()
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

    (* query default class *)
    fun defaultClassOf p = let
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
	    SOME e => sfx_loop e
	  | NONE => gen_loop (!gen_classifiers)
    end

    fun smlrule srq { spec = (name, mkpath, _), context, mkNativePath } =
	({ smlfiles = [(mkpath name, srq)], cmfiles = [] }, [])
    fun cmrule { spec = (name, mkpath, _), context, mkNativePath } =
	({ smlfiles = [], cmfiles = [mkpath name] }, [])

    fun expand { error, spec, context } = let
	fun mkNativePath s = SrcPath.native { context = context, spec = s }
	fun class2rule class =
	    case StringMap.find (!classes, class) of
		SOME rule => rule
	      | NONE => (error (concat ["unknown class \"", class, "\""]);
			 smlrule Sharing.DONTCARE)
	fun expand1 (spec as (name, _, co)) = let
	    val rule =
		case co of
		    SOME c0 => class2rule (String.map Char.toLower c0)
		  | NONE =>
			(case defaultClassOf name of
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

structure Tools : TOOLS = PrivateTools
