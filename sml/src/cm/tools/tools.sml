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

    (* install "ML Source" class *)
    val registerSmlClass : class * bool option -> unit

    (* install "CM Group" class *)
    val registerGroupClass : class -> unit

    (* classifiers are used when the class is not given explicitly *)
    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of string -> class option

    (* make a classifier which looks for a specific file name suffix *)
    val stdSfxClassifier : { sfx: string, class: class } -> classifier

    (* install a classifier *)
    val registerClassifier : classifier -> unit

    (* query default class *)
    val defaultClassOf : fname -> class option
end

signature PRIVATETOOLS = sig

    include TOOLS

    type smlsource =
	{ sourcepath: AbsPath.t, history: class list, share: bool option }

    datatype expansion =
	SMLSOURCE of smlsource
      | GROUP of AbsPath.t

    datatype private_rule =
	ISSML of bool option
      | ISGROUP
      | ISTOOL of class * rule

    val expand : (string -> unit) -> AbsPath.t * class option -> expansion list
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
	{ sourcepath: AbsPath.t, history: class list, share: bool option }

    datatype expansion =
	SMLSOURCE of smlsource
      | GROUP of AbsPath.t

    datatype private_rule =
	ISSML of bool option
      | ISGROUP
      | ISTOOL of class * rule

    val classes : private_rule StringMap.map ref = ref (StringMap.empty)

    fun registerClass (class, rule) =
	classes := StringMap.insert (!classes, class, ISTOOL (class, rule))

    fun registerSmlClass (class, share) =
	classes := StringMap.insert (!classes, class, ISSML share)

    fun registerGroupClass class =
	classes := StringMap.insert (!classes, class, ISGROUP)

    (* classifiers are used when the class is not given explicitly *)
    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of string -> class option

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
			 ISSML NONE)

	(* apply a rule to a path within a given context *)
	fun apply (rule, p, c) = let
	    fun rctxt rf = let
		val dir = AbsPath.contextName c
		val cwd = OS.FileSys.getDir ()
		fun doit () =
		    (OS.FileSys.chDir dir; rf () before OS.FileSys.chDir cwd)
	    in
		(Interrupt.guarded doit)
		handle ToolError { tool, msg } =>
		            (OS.FileSys.chDir cwd;
			     error (concat ["tool \"", tool, "\" failed: ",
					    msg]);
			     [])
		     | exn => (OS.FileSys.chDir cwd; raise exn)
	    end
	in
	    rule (p, rctxt)
	end

	fun expand' context = let
	    fun loop (acc, []) = rev acc
	      | loop (acc, ((p, c), history) :: t) = let
		    fun step (ISSML share) =
			let
			    val ap = AbsPath.native { context = context,
						      spec = p }
			    val src = { sourcepath = ap,
				        history = rev history,
					share = share }
			in
			    loop (SMLSOURCE src :: acc, t)
			end
		      | step ISGROUP = let
			    val ap = AbsPath.native { context = context,
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
			       | NONE => step (ISSML NONE))
		end
	in
	    fn l => loop ([], l)
	end

	fun expand0 (ap, SOME class) =
	    (case class2rule class of
		 ISSML share =>
		     [SMLSOURCE { sourcepath = ap, history = [],
				  share = share }]
	       | ISGROUP =>
		     [GROUP ap]
	       | ISTOOL (class, rule) => let
		     val c = AbsPath.context ap
		     val l = apply (rule, AbsPath.spec ap, c)
		     val l' = map (fn i => (i, [class])) l
		 in
		     expand' (AbsPath.context ap) l'
		 end)
	  | expand0 (ap, NONE) =
		 expand' (AbsPath.context ap) [((AbsPath.spec ap, NONE), [])]
    in
	expand0
    end

    (* registering standard classes and classifiers *)
    local
	fun sfx (s, c) =
	    registerClassifier (stdSfxClassifier { sfx = s, class = c })
    in
	val _ = registerSmlClass ("sml", NONE)
	val _ = registerSmlClass ("shared", SOME true)
	val _ = registerSmlClass ("private", SOME false)
	val _ = registerGroupClass "cm"
	    
	val _ = sfx ("sml", "sml")
	val _ = sfx ("sig", "sml")
	val _ = sfx ("cm", "cm")
    end
end

structure Tools :> TOOLS = PrivateTools
