(*
 * Target expansion and CM tools.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature TOOLS = sig

    type fname = string
    type class = string

    exception ToolError of { tool: string, msg: string }

    type item = fname * class option

    (* A rule takes a file name (relative to the directory of the
     * corresponding description file) and a rulecontext.  In general,
     * when coding a rule one would write a rule function and pass it to
     * the context, which will temporarily change the current working
     * directory to the one that holds the description file ("the context").
     * If this is not necessary for the rule to work correctly, then
     * one can simply ignore the context (this saves system call overhead
     * during dependency analysis). *)
    type rulefn = unit -> item list
    type rulecontext = rulefn -> item list
    type rule = fname * rulecontext -> item list

    (* install a class *)
    val registerClass : class * rule -> unit

    (* This is a local copy of Sharing.request to allow this signature to be
     * useful without access to Sharing.request. (We still need a separate
     * structure "Sharing" because otherwise we get a dependency cycle.) *)
    datatype sharingrequest = PRIVATE | SHARED | DONTCARE

    (* install "ML Source" class *)
    val registerSmlClass : class * sharingrequest -> unit

    (* install "CM Group" class *)
    val registerGroupClass : class -> unit

    (* classifiers are used when the class is not given explicitly *)
    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of fname -> class option

    (* make a classifier which looks for a specific file name suffix *)
    val stdSfxClassifier : { sfx: string, class: class } -> classifier

    (* two standard ways of dealing with filename extensions... *)
    datatype extensionStyle =
	EXTEND of string list
      | REPLACE of string list * string list

    type cmdGetterSetter = string option -> string

    val newCmdGetterSetter : string * string -> cmdGetterSetter

    val registerStdShellCmdTool : { tool : string,
				    class : string,
				    suffixes : string list,
				    command : cmdGetterSetter,
				    extensionStyle : extensionStyle,
				    sml : bool } -> unit

    (* perform filename extension *)
    val extend : extensionStyle -> fname -> fname list

    (* check for outdated files *)
    val outdated : string -> fname list * fname -> bool

    (* install a classifier *)
    val registerClassifier : classifier -> unit

    (* query default class *)
    val defaultClassOf : fname -> class option
end

signature PRIVATETOOLS = sig

    include TOOLS

    type smlsource =
	{ sourcepath: SrcPath.t, history: class list,
	  sh_spec: Sharing.request }

    datatype expansion =
	SMLSOURCE of smlsource
      | GROUP of SrcPath.t

    datatype private_rule =
	ISSML of Sharing.request
      | ISGROUP
      | ISTOOL of class * rule

    val expand : (string -> unit) -> SrcPath.t * class option -> expansion list
end

structure PrivateTools :> PRIVATETOOLS = struct

    type fname = string
    type class = string

    exception ToolError of { tool: string, msg: string }

    type item = fname * class option

    type rulefn = unit -> item list
    type rulecontext = rulefn -> item list
    type rule = fname * rulecontext -> item list

    type smlsource =
	{ sourcepath: SrcPath.t, history: class list,
	  sh_spec: Sharing.request }

    datatype expansion =
	SMLSOURCE of smlsource
      | GROUP of SrcPath.t

    datatype private_rule =
	ISSML of Sharing.request
      | ISGROUP
      | ISTOOL of class * rule

    datatype sharingrequest = PRIVATE | SHARED | DONTCARE

    val classes : private_rule StringMap.map ref = ref (StringMap.empty)

    fun registerClass (class, rule) =
	classes := StringMap.insert (!classes, class, ISTOOL (class, rule))

    fun registerSmlClass (class, shrq) = let
	(* This seems clumsy, but we need to "translate" the local
	 * sharingrequest value into a Sharing.request value... *)
	val shrq =
	    case shrq of
		PRIVATE => Sharing.PRIVATE
	      | SHARED => Sharing.SHARED
	      | DONTCARE => Sharing.DONTCARE
    in
	classes := StringMap.insert (!classes, class, ISSML shrq)
    end

    fun registerGroupClass class =
	classes := StringMap.insert (!classes, class, ISGROUP)

    (* classifiers are used when the class is not given explicitly *)
    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of fname -> class option

    (* make a classifier which looks for a specific file name suffix *)
    fun stdSfxClassifier { sfx, class } =
	SFX_CLASSIFIER (fn e => if sfx = e then SOME class else NONE)

    (* install a classifier *)
    val sfx_classifiers: (string -> class option) list ref = ref []
    val gen_classifiers: (fname -> class option) list ref = ref []

    local
	fun add (x, r) = r := x :: (!r)
    in
	fun registerClassifier (SFX_CLASSIFIER c) = add (c, sfx_classifiers)
	  | registerClassifier (GEN_CLASSIFIER c) = add (c, gen_classifiers)
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

    fun expand error = let

	(* get the rule corresponding to a given class *)
	fun class2rule class =
	    case StringMap.find (!classes, class) of
		SOME rule => rule
	      | NONE => (error (concat ["unknown class \"", class, "\""]);
			 ISSML Sharing.DONTCARE)

	(* apply a rule to a path within a given context *)
	fun apply (rule, p, c) = let
	    fun rctxt rf = let
		val dir = SrcPath.contextName c
		val cwd = OS.FileSys.getDir ()
	    in
		SafeIO.perform { openIt = fn () => OS.FileSys.chDir dir,
				 closeIt = fn () => OS.FileSys.chDir cwd,
				 work = rf,
				 cleanup = fn () => () }
		handle ToolError { tool, msg } =>
		    (error (concat ["tool \"", tool, "\" failed: ", msg]); [])
	    end
	in
	    rule (p, rctxt)
	end

	fun expand' context = let
	    fun loop (acc, []) = rev acc
	      | loop (acc, ((p, c), history) :: t) = let
		    fun step (ISSML shrq) =
			let
			    val ap = SrcPath.native { context = context,
						      spec = p }
			    val src = { sourcepath = ap,
				        history = rev history,
					sh_spec = shrq }
			in
			    loop (SMLSOURCE src :: acc, t)
			end
		      | step ISGROUP = let
			    val ap = SrcPath.native { context = context,
						      spec = p }
			in
			    loop (GROUP ap :: acc, t)
			end
		      | step (ISTOOL (class, rule)) = let
			    val l = apply (rule, p, context)
			    val l' = map (fn i => (i, class :: history)) l
			in
			    loop (acc, l' @ t)
			end
		in
		    case c of
			SOME class => step (class2rule class)
		      | NONE =>
			    (case defaultClassOf p of
				 SOME class => step (class2rule class)
			       | NONE => step (ISSML Sharing.DONTCARE))
		end
	in
	    fn l => loop ([], l)
	end

	fun expand0 (ap, NONE) =
	    expand' (SrcPath.contextOf ap) [((SrcPath.specOf ap, NONE), [])]
	  | expand0 (ap, SOME class0) = let
		(* classes are case-insensitive, internally we use lowercase *)
		val class = String.map Char.toLower class0
	    in
		case class2rule class of
		    ISSML shrq =>
			[SMLSOURCE { sourcepath = ap, history = [],
				     sh_spec = shrq }]
		  | ISGROUP =>
			[GROUP ap]
		  | ISTOOL (class, rule) => let
			val c = SrcPath.contextOf ap
			val l = apply (rule, SrcPath.specOf ap, c)
			val l' = map (fn i => (i, [class])) l
		    in
			expand' (SrcPath.contextOf ap) l'
		    end
	    end
    in
	expand0
    end

    datatype extensionStyle =
	EXTEND of string list
      | REPLACE of string list * string list

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

    type cmdGetterSetter = string option -> string

    fun newCmdGetterSetter sp = EnvConfig.getSet (EnvConfig.new SOME sp)

    fun registerStdShellCmdTool arg = let
	val { tool, class, suffixes, command, extensionStyle, sml } = arg
	fun rule (f, ctxt) = let
	    val targetfiles = extend extensionStyle f
	    val mkTarget =
		if sml then (fn tf => (tf, SOME "sml"))
		else (fn tf => (tf, NONE))
	    val targets = map mkTarget targetfiles
	    fun runcmd () = let
		val cmd = concat [command NONE, " ", f]
		val _ = Say.vsay ["[", cmd, "]\n"]
	    in
		if OS.Process.system cmd = OS.Process.success then ()
		else raise ToolError { tool = tool, msg = cmd }
	    end
	    fun rfun () =
		(if outdated tool (targetfiles, f) then runcmd ()
		 else ();
		 targets)
	in
	    ctxt rfun
	end

	fun sfx s =
	    registerClassifier (stdSfxClassifier { sfx = s, class = class })
    in
	registerClass (class, rule);
	app sfx suffixes
    end

    (* registering standard classes and classifiers *)
    local
	fun sfx (s, c) =
	    registerClassifier (stdSfxClassifier { sfx = s, class = c })
    in
	val _ = registerSmlClass ("sml", DONTCARE)
	val _ = registerSmlClass ("shared", SHARED)
	val _ = registerSmlClass ("private", PRIVATE)
	val _ = registerGroupClass "cm"
	    
	val _ = sfx ("sml", "sml")
	val _ = sfx ("sig", "sml")
	val _ = sfx ("cm", "cm")
    end
end

structure Tools : TOOLS = PrivateTools
