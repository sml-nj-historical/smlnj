signature TOOLS = sig

    type fname = string
    type class = string

    exception UnknownClass of class
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

    type primitive = Primitive.primitive

    type smlsource =
	{ sourcepath: AbsPath.t, history: class list, share: bool option }

    datatype expansion =
	PRIMITIVE of primitive
      | SMLSOURCE of smlsource
      | GROUP of AbsPath.t

    datatype private_rule =
	ISPRIMITIVE
      | ISSML of bool option
      | ISGROUP
      | ISTOOL of class * rule

    val registerPrimitiveClass : class -> unit

    val expand : AbsPath.t * class option -> expansion list
end

structure PrivateTools :> PRIVATETOOLS = struct

    type fname = string
    type class = string

    exception UnknownClass of class
    exception ToolError of { tool: string, msg: string }

    type item = fname * class option

    type rulefn = unit -> item list
    type rulecontext = rulefn -> item list
    type rule = fname * rulecontext -> item list

    type primitive = Primitive.primitive

    type smlsource =
	{ sourcepath: AbsPath.t, history: class list, share: bool option }

    datatype expansion =
	PRIMITIVE of primitive
      | SMLSOURCE of smlsource
      | GROUP of AbsPath.t

    datatype private_rule =
	ISPRIMITIVE
      | ISSML of bool option
      | ISGROUP
      | ISTOOL of class * rule

    val classes : private_rule StringMap.map ref = ref (StringMap.empty)

    fun registerClass (class, rule) =
	classes := StringMap.insert (!classes, class, ISTOOL (class, rule))

    fun registerSmlClass (class, share) =
	classes := StringMap.insert (!classes, class, ISSML share)

    fun registerGroupClass class =
	classes := StringMap.insert (!classes, class, ISGROUP)

    fun registerPrimitiveClass class =
	classes := StringMap.insert (!classes, class, ISPRIMITIVE)

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

    (* get the rule corresponding to a given class *)
    fun class2rule class =
	case StringMap.find (!classes, class) of
	    SOME rule => rule
	  | NONE => raise UnknownClass class

    (* apply a rule to a path within a given context *)
    fun apply (rule, p, c) = let
	fun rctxt rf = let
	    val dir = AbsPath.contextName c
	    val cwd = OS.FileSys.getDir ()
	    fun doit () =
		(OS.FileSys.chDir dir; rf () before OS.FileSys.chDir cwd)
	in
	    (Interrupt.guarded doit)
	    handle exn => (OS.FileSys.chDir cwd; raise exn)
	end
    in
	rule (p, rctxt)
    end

    fun expand' context = let
	fun loop (acc, []) = rev acc
	  | loop (acc, ((p, c), history) :: t) = let
		fun step ISPRIMITIVE =
		    loop (PRIMITIVE (Primitive.fromString p) :: acc, t)
		  | step (ISSML share) = let
			val ap = AbsPath.native { context = context, spec = p }
			val src = { sourcepath = ap,
				    history = rev history,
				    share = share }
		    in
			loop (SMLSOURCE src :: acc, t)
		    end
		  | step ISGROUP = let
			val ap = AbsPath.native { context = context, spec = p }
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
		  | NONE => step (ISSML NONE)
	    end
    in
	fn l => loop ([], l)
    end

    fun expand (ap, SOME class) =
	(case class2rule class of
	     ISPRIMITIVE =>
		 [PRIMITIVE (Primitive.fromString (AbsPath.spec ap))]
	   | ISSML share =>
		 [SMLSOURCE { sourcepath = ap, history = [], share = share }]
	   | ISGROUP =>
		 [GROUP ap]
	   | ISTOOL (class, rule) => let
		 val c = AbsPath.context ap
		 val l = apply (rule, AbsPath.spec ap, c)
		 val l' = map (fn i => (i, [class])) l
	     in
		 expand' (AbsPath.context ap) l'
	     end)
      | expand (ap, NONE) =
	     expand' (AbsPath.context ap) [((AbsPath.spec ap, NONE), [])]
end

structure Tools :> TOOLS = PrivateTools
