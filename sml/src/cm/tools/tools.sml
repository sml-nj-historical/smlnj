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
    type presrcpath

    type rebindings = { anchor: string, value: presrcpath } list

    val nativeSpec : srcpath -> string

    val nativePre : presrcpath -> string

    val srcpath : presrcpath -> srcpath

    exception ToolError of { tool: string, msg: string }

    type pathmaker = string -> presrcpath

    (* case-by-case parameters that can be passed to tools... *)
    datatype toolopt =
	STRING of { name: string, mkpath: pathmaker }
      | SUBOPTS of { name: string, opts: toolopts }
    withtype toolopts = toolopt list

    type tooloptcvt = toolopts option -> toolopts option

    (* A member specification consists of the actual string, an optional
     * class name, (optional) tool options, a function to convert a
     * string to its corresponding srcpath, and information about whether
     * or not this source is an "original" source or a derived source
     * (i.e., output of some tool). *)
    type spec = { name: string,
		  mkpath: pathmaker,
		  class: class option,
		  opts: toolopts option,
		  derived: bool }

    type setup = string option * string option (* (pre, post) *)

    (* The goal of applying tools to members is to obtain an "expansion",
     * i.e., a list of ML-files and a list of .cm-files.  We also
     * obtain a list of "sources".  This is used to implement CM.sources,
     * i.e., to generate dependency information etc. *)
    type expansion =
	 { smlfiles: (srcpath * Sharing.request * setup) list,
	   cmfiles: (srcpath * Version.t option * rebindings) list,
	   sources: (srcpath * { class: string, derived: bool}) list }

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

    (* two standard ways of dealing with filename extensions...
     * (Tool options can be calculated from the options that we have.) *)
    datatype extensionStyle =
	EXTEND of (string * class option * tooloptcvt) list
      | REPLACE of string list * (string * class option * tooloptcvt) list

    (* perform filename extension *)
    val extend : extensionStyle ->
		 (string * toolopts option) ->
		 (string * class option * toolopts option) list

    (* check for outdated files; the pathname strings must be in
     * native syntax! *)
    val outdated : string -> string list * string -> bool

    (* Alternative way of checking for outdated-ness using a "witness"
     * file.  The idea is that if both tgt (target) and wtn (witness)
     * exist, then tgt is considered outdated if wtn is older than src.
     * Otherwise, if tgt exists but wtn does not, then tgt is considered
     * outdated if it is older than src.  If tgt does not exist, it is
     * always considered outdated. *)
    val outdated' : string ->
		    { src: string, wtn: string, tgt: string } -> bool

    (* open output file; make all necessary directories for it *)
    val openTextOut : string -> TextIO.outstream

    (* make all directories leading up to a given file; the file itself
     * is to be left alone *)
    val makeDirs : string -> unit

    (* install a classifier *)
    val registerClassifier : classifier -> unit

    (* grab all named options... *)
    val parseOptions :
	{ tool : string, keywords : string list, options : toolopts } ->
	{ matches : string -> toolopts option, restoptions : string list }
end

signature PRIVATETOOLS = sig
    include CORETOOLS where type srcpath = SrcPath.file
		      where type presrcpath = SrcPath.prefile

    type registry

    val newRegistry : unit -> registry

    val expand : { error: string -> unit,
		   local_registry : registry,
		   spec: spec,
		   context: SrcPath.dir,
		   load_plugin: SrcPath.dir -> string -> bool }
	-> expansion

    val defaultClassOf : (string -> bool) -> string -> class option

    val withPlugin : SrcPath.file -> (unit -> 'a) -> 'a
end

signature TOOLS = sig
    include CORETOOLS

    (* CM's say and vsay functions *)
    val say : string list -> unit
    val vsay : string list -> unit

    (* Get an anchor-configurable command name. *)
    val mkCmdName : string -> string

    (* Register a "standard" tool based on some shell command. *)
    val registerStdShellCmdTool : { tool: string,
				    class: string,
				    suffixes: string list,
				    cmdStdPath: string,
				    extensionStyle: extensionStyle,
				    template: string option,
				    dflopts: toolopts } -> unit

    (* query default class *)
    val defaultClassOf : string -> class option
end

structure PrivateTools :> PRIVATETOOLS = struct

    type class = string

    type srcpath = SrcPath.file
    type presrcpath = SrcPath.prefile
    type rebindings = SrcPath.rebindings

    val nativeSpec = SrcPath.osstring_relative

    val nativePre = SrcPath.osstring_prefile

    val srcpath = SrcPath.file

    exception ToolError of { tool: string, msg: string }

    type pathmaker = string -> presrcpath

    datatype toolopt =
	STRING of { name: string, mkpath: pathmaker }
      | SUBOPTS of { name: string, opts: toolopts }
    withtype toolopts = toolopt list

    type tooloptcvt = toolopts option -> toolopts option

    type spec = { name: string,
		  mkpath: pathmaker,
		  class: class option,
		  opts: toolopts option,
		  derived: bool }

    type setup = string option * string option

    type expansion =
	 { smlfiles: (srcpath * Sharing.request * setup) list,
	   cmfiles: (srcpath * Version.t option * rebindings) list,
	   sources: (srcpath * { class: string, derived: bool}) list }

    type partial_expansion = expansion * spec list

    type rulefn = unit -> partial_expansion
    type rulecontext = rulefn -> partial_expansion
    type rule =
	{ spec: spec, mkNativePath: pathmaker, context: rulecontext } ->
	partial_expansion

    type registry = { classes : rule StringMap.map ref,
		      sfx_classifiers : (string -> class option) ref,
		      gen_classifiers : (string -> class option) ref }

    fun layer (look1, look2) s = case look1 s of NONE => look2 s | x => x

    fun newRegistry () =  { classes = ref StringMap.empty,
			    sfx_classifiers = ref (fn _ => NONE),
			    gen_classifiers = ref (fn _ => NONE) } : registry

    (* Three registries:
     *  1. global: where globally available tools are registered and found.
     *  2. local: where locally available tools are found;
     *            the local registry is being set anew every time "expand"
     *            is being called; each instance of a local registry belongs
     *            to one description file that is being processed.
     *  3. plugin registries: mapping from tool implementations (indexed
     *            by their respective description files) to that tool's
     *            registry; here is where local tools register themselves;
     *            the rule for the "tool" class causes the tool to register
     *            itself if it has not already done so and then merges
     *            the contents of the tool's registry into the current
     *            local registry.
     * These complications exist because tools register themselves via
     * side-effects. *)

    val global_registry = newRegistry ()

    val local_registry : registry ref = ref (newRegistry ())

    val plugin_registries : registry SrcPathMap.map ref = ref SrcPathMap.empty

    val current_plugin : SrcPath.file option ref = ref NONE

    local
	fun registry sel cvt s = let
	    val get = cvt o ! o sel
	in
	    layer (get (!local_registry), get global_registry) s
	end
	fun curry f x y = f (x, y)
    in
        val classes = registry #classes (curry StringMap.find)
        val sfx_classifiers = registry #sfx_classifiers (fn x => x)
	val gen_classifiers = registry #gen_classifiers (fn x => x)
    end

    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of string -> class option

    fun stdSfxClassifier { sfx, class } =
	SFX_CLASSIFIER (fn e => if sfx = e then SOME class else NONE)

    local
	fun upd sel augment = let
	    val rf =
		sel (case !current_plugin of
			 NONE => global_registry
		       | SOME p =>
			 (case SrcPathMap.find (!plugin_registries, p) of
			      SOME r => r
			    | NONE => let
				  val r = newRegistry ()
			      in
				  plugin_registries :=
				  SrcPathMap.insert (!plugin_registries, p, r);
				  r
			      end))
	in
	    rf := augment (!rf)
	end
    in
        fun registerClass (class, rule) =
	    upd #classes (fn m => StringMap.insert (m, class, rule))
	fun registerClassifier (SFX_CLASSIFIER c) =
	    upd #sfx_classifiers (fn c' => layer (c, c'))
	  | registerClassifier (GEN_CLASSIFIER c) =
	    upd #gen_classifiers (fn c' => layer (c, c'))

	fun transfer_local p = let
	    val lr = !local_registry
	in
	    case SrcPathMap.find (!plugin_registries, p) of
		NONE => ()
	      | SOME pr => let
		    fun upd sel join = sel lr := join (! (sel pr), ! (sel lr))
		in
		    upd #classes (StringMap.unionWith #1);
		    upd #sfx_classifiers layer;
		    upd #gen_classifiers layer
		end
	end

	fun withPlugin p thunk =
	    SafeIO.perform { openIt = fn () => !current_plugin before
					       current_plugin := SOME p,
			     closeIt = fn prev => (transfer_local p;
						   current_plugin := prev),
			     work = fn _ => thunk (),
			     cleanup = fn _ => () }
    end

    datatype extensionStyle =
	EXTEND of (string * class option * tooloptcvt) list
      | REPLACE of string list * (string * class option * tooloptcvt) list

    fun extend (EXTEND l) (f, too) =
	map (fn (s, co, toc) => (concat [f, ".", s], co, toc too)) l
      | extend (REPLACE (ol, nl)) (f, too) = let
	    val { base, ext } = OS.Path.splitBaseExt f
	    fun join b (e, co, toc) =
		(OS.Path.joinBaseExt { base = b, ext = SOME e }, co, toc too)
	    fun gen b = map (join b) nl
	    fun sameExt (e1: string) (e2: string) = e1 = e2
	in
	    case ext of
		NONE => gen base
	      | SOME e =>
		    if List.exists (sameExt e) ol then gen base else gen f
	end

    local
	fun timex f =
	    (OS.FileSys.modTime f, true)
	    handle _ => (Time.zeroTime, false)
	val op < = Time.<
	fun olderThan t f = OS.FileSys.modTime f < t
	fun cannotAccess tool f =
	    raise ToolError { tool = tool, msg = "cannot access " ^ f }
    in
        fun outdated tool (l, f) = let
	    val (ftime, fexists) = timex f
	in
	    (List.exists (olderThan ftime) l)
	    handle _ => if fexists then true else cannotAccess tool f
	end

	fun outdated' tool { src, wtn, tgt } = let
	    val (st, se) = timex src
	    val (tt, te) = timex tgt
	in
	    if not se then
		if te then false else cannotAccess tool src
	    else if te then
		let val (wt, we) = timex wtn
		in
		    if we then wt < st else tt < st
		end
	    else true
	end
    end

    val openTextOut = AutoDir.openTextOut
    val makeDirs = AutoDir.makeDirs

    fun globally lp arg =
	SafeIO.perform { openIt = fn () => !current_plugin before
					   current_plugin := NONE,
			 closeIt = fn prev => current_plugin := prev,
			 work = fn _ => lp arg,
			 cleanup = fn _ => () }

    (* query default class *)
    fun defaultClassOf load_plugin p = let
	fun sfx_gen_check e =
	    case sfx_classifiers e of
		SOME c => SOME c
	      | NONE => gen_classifiers p

    in
	case OS.Path.ext p of
	    SOME e =>
	    (case sfx_gen_check e of
		 SOME c => SOME c
	       | NONE => let
		     val plugin = concat ["$/", e, "-ext.cm"]
		 in
		     if globally load_plugin plugin then sfx_gen_check e
		     else NONE
		 end)
	  | NONE => gen_classifiers p
    end

    fun parseOptions { tool, keywords, options } = let
	fun err m = raise ToolError { tool = tool, msg = m }
	fun isKW kw = List.exists (fn kw' => kw = kw') keywords
	fun loop ([], m, ro) = { matches = fn kw => StringMap.find (m, kw),
				 restoptions = rev ro }
	  | loop (STRING { name, ... } :: t, m, ro) = loop (t, m, name :: ro)
	  | loop (SUBOPTS { name, opts } :: t, m, ro) =
	    if not (isKW name) then
		raise err (concat ["keyword option `", name,
				   "' not recognized"])
	    else (case StringMap.find (m, name) of
		      SOME _ => err (concat ["keyword option `", name,
					     "' specified more than once"])
		    | NONE => loop (t, StringMap.insert (m, name, opts), ro))
    in
	loop (options, StringMap.empty, [])
    end

    fun smlrule { spec, context, mkNativePath } = let
	val { name, mkpath, opts = oto, derived, ... } : spec = spec
	val tool = "sml"
	fun err s = raise ToolError { tool = tool, msg = s }
	val kw_setup = "setup"
	val (srq, setup) =
	    case oto of
		NONE => (Sharing.DONTCARE, (NONE, NONE))
	      | SOME to => let
		    val { matches, restoptions } =
			parseOptions { tool = tool,
				       keywords = [kw_setup],
				       options = to }
		    val srq =
			case restoptions of
			    [] => Sharing.DONTCARE
			  | ["shared"] => Sharing.SHARED
			  | ["private"] => Sharing.PRIVATE
			  | _ => err "invalid option(s)"
		    val setup =
			case matches kw_setup of
			    NONE => (NONE, NONE)
			  | SOME [] => (NONE, NONE)
			  | SOME [STRING s] => (SOME (#name s), NONE)
			  | SOME [SUBOPTS { name = "pre",
					    opts = [STRING pre] }] =>
			    (SOME (#name pre), NONE)
			  | SOME [SUBOPTS { name = "post",
					    opts = [STRING post] }] =>
			    (NONE, SOME (#name post))
			  | (SOME [SUBOPTS { name = "pre",
					     opts = [STRING pre] },
				   SUBOPTS { name = "post",
					     opts = [STRING post] }] |
			     SOME [SUBOPTS { name = "post",
					     opts = [STRING post] },
				   SUBOPTS { name = "pre",
					     opts = [STRING pre] }]) =>
			    (SOME (#name pre), SOME (#name post))
			  | _ => err "invalid setup spec"
		in
		    (srq, setup)
		end
	val p = srcpath (mkpath name)
    in
	({ smlfiles = [(p, srq, setup)],
	   sources = [(p, { class = "sml", derived = derived })],
	   cmfiles = [] },
	 [])
    end
    fun cmrule { spec, context, mkNativePath } = let
	val { name, mkpath, opts = oto, derived, ... } : spec = spec
	fun err m = raise ToolError { tool = "cm", msg = m }
	fun proc_opts (rb, vrq, []) = (rb, vrq)
	  | proc_opts (_, _, STRING _ :: _) = err "ill-formed option"
	  | proc_opts (rb, vrq, SUBOPTS { name = "version", opts } :: r)  =
	    let fun ill () = err "ill-formed version specification"
	    in
		case (vrq, opts) of
		    (SOME _, _) =>
		    err "version cannot be specified more than once"
		  | (NONE, [STRING { name, ... }]) =>
		    (case Version.fromString name of
			 NONE => ill ()
		       | SOME v => proc_opts (rb, SOME v, r))
		  | _ => ill ()
	    end
	  | proc_opts (rb, vrq, SUBOPTS { name = "bind", opts } :: r) =
	    (case opts of
		 [SUBOPTS { name = "anchor", opts = [STRING { name, ... }] },
		  SUBOPTS { name = "value", opts = [STRING v] }] =>
		 proc_opts ({ anchor = name, value = #mkpath v (#name v) }
			    :: rb,
			    vrq, r)
	       | _ => err "ill-formed bind specification")
	  | proc_opts (_, _, SUBOPTS { name, ... } :: _) =
	    err ("unknown option: " ^ name)
	val (rb, vrq) = case oto of
			    NONE => ([], NONE)
			  | SOME l => proc_opts ([], NONE, l)
	val p = srcpath (mkpath name)
    in
	({ smlfiles = [],
	   sources = [(p, { class = "cm", derived = derived })],
	   cmfiles = [(p, vrq, rev rb)] },
	 [])
    end

    fun expand { error, local_registry = lr, spec, context, load_plugin } = let
	fun mkNativePath s =
	    SrcPath.native { err = error } { context = context, spec = s }
	fun class2rule class =
	    case classes class of
		SOME rule => rule
	      | NONE => let
		    val base = concat ["$/", class, "-tool"]
		    val plugin = OS.Path.joinBaseExt { base = base,
						       ext = SOME "cm" }
		    fun complain () =
			(error (concat ["unknown class \"", class, "\""]);
			 smlrule)
		in
		    if globally (load_plugin context) plugin then
			case classes class of
			    SOME rule => rule
			  | NONE => complain ()
		    else complain ()
		end

	fun expand1 (spec as { name, class = co, ... }) = let
	    val rule =
		case co of
		    SOME c0 => class2rule (String.map Char.toLower c0)
		  | NONE =>
			(case defaultClassOf (load_plugin context) name of
			     SOME c => class2rule c
			   | NONE => smlrule)
	    fun rcontext rf = let
		val dir = SrcPath.osstring_dir context
		val cwd = OS.FileSys.getDir ()
	    in
		SafeIO.perform { openIt = fn () => OS.FileSys.chDir dir,
				 closeIt = fn () => OS.FileSys.chDir cwd,
				 work = rf,
				 cleanup = fn _ => () }
	    end
	in
	    rule { spec = spec, context = rcontext,
		   mkNativePath = mkNativePath }
	    handle ToolError { tool, msg } =>
		   (error (concat ["tool \"", tool, "\" failed: ", msg]);
		    ({ smlfiles = [], cmfiles = [], sources = [] }, []))
	end
	fun loop (expansion, []) = expansion
	  | loop ({ smlfiles, cmfiles, sources }, item :: items) = let
		val ({ smlfiles = sfl, cmfiles = cfl, sources = sl }, il) =
		    expand1 item
	    in
		loop ({ smlfiles = smlfiles @ sfl,
			cmfiles = cmfiles @ cfl,
			sources = sources @ sl },
		      il @ items)
	    end
    in
	SafeIO.perform { openIt = fn () => !local_registry
					   before local_registry := lr,
			 closeIt = fn prev => local_registry := prev,
			 work = fn _ => loop ({ smlfiles = [], cmfiles = [],
						sources = [] },
					      [spec]),
			 cleanup = fn _ => () }
    end

    local
	fun sfx (s, c) =
	    registerClassifier (stdSfxClassifier { sfx = s, class = c })
    in
	val _ = registerClass ("sml", smlrule)
	val _ = registerClass ("cm", cmrule)

	val _ = sfx ("sml", "sml")
	val _ = sfx ("sig", "sml")
	val _ = sfx ("fun", "sml")
	val _ = sfx ("cm", "cm")
    end
end

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
