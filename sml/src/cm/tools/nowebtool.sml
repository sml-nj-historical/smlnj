(*
 * A tool for source code written using Norman Ramsey's "noweb".
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure NowebTool = struct
    local
	open Tools

	val tool = "Noweb"
	val class = "noweb"

	val stdCmdPath = "notangle"

	val kw_target = "target"	(* "master" keyword *)

	val kw_name = "name"		(* sub-keywords... *)
	val kw_root = "root"
	val kw_class = "class"
	val kw_options = "options"
	val kw_lineformat = "lineformat"
	val kw_cpif = "cpif"
	val kwl =
	    [kw_name, kw_root, kw_class, kw_options, kw_lineformat, kw_cpif]

	fun err msg = raise ToolError { tool = tool, msg = msg }
	fun kwerr what kw = err (concat [what, " keyword `", kw, "'"])
	fun badkw kw = kwerr "unknown" kw
	fun misskw kw = kwerr "missing" kw
	fun badspec kw = kwerr "bad specification for " kw

	structure StringMap = RedBlackMapFn
	    (struct
	         type ord_key = string
		 val compare = String.compare
	     end)

	val lnr = ref (foldl StringMap.insert' StringMap.empty
		       [("sml", "(*#line %L \"%F\"*)"),
			("cm", "#line %L %F%N")])

	fun rule { spec, context, mkNativePath } = let
	    val { name = str, mkpath, opts = too, derived, ... } : spec = spec
	    val p = mkpath str
	    val sname = nativeSpec p
	    fun oneTarget (tname, rname, tclass, topts, lf, cpif) = let
		fun runcmd () = let
		    val cmdname = mkCmdName stdCmdPath
		    fun number f = concat ["-L'", f, "' "]
		    val nonumber = "-L'' "
		    val fmtopt =
			case lf of
			    NONE => let
				fun classNumbering c =
				    case StringMap.find (!lnr, c) of
					NONE => nonumber
				      | SOME f => number f
			    in
				case tclass of
				    SOME c => classNumbering c
				  | NONE =>
				    (case defaultClassOf tname of
					 SOME c => classNumbering c
				       | NONE => "-L'' ")
			    end
			  | SOME f => number f
		    val redirect = if cpif then "| cpif " else ">"
		    val cmd = concat [cmdname, " ", fmtopt, "-R'", rname, "' ",
				      sname, " ", redirect, tname]
				
		in
		    vsay ["[", cmd, "]\n"];
		    if OS.Process.system cmd = OS.Process.success then ()
		    else err cmd
		end
	    in
		if outdated tool ([tname], sname) then runcmd ()
		else ();
		{ name = tname, mkpath = mkNativePath,
		  class = tclass, opts = topts, derived = true }
	    end

	    fun simpleTarget { name, mkpath } = let
		val tname = nativeSpec (mkpath name)
	    in
		oneTarget (tname, tname, NONE, NONE, NONE, true)
	    end

	    fun oneOpt (STRING x) = simpleTarget x
	      | oneOpt (SUBOPTS { name, opts }) = let
		    fun subopts [STRING x] = simpleTarget x
		      | subopts opts = let
			    val { matches, restoptions } =
				parseOptions { tool = tool, keywords = kwl,
					       options = opts }
			    fun fmatch kw =
				case matches kw of
				    NONE => misskw kw
				  | SOME [STRING { name, mkpath }] =>
				    nativeSpec (mkpath name)
				  | _ => badspec kw
			    fun smatch kw =
				case matches kw of
				    NONE => NONE
				  | SOME [STRING { name, ... }] => SOME name
				  | _ => badspec kw
			in
			    case restoptions of
				[] => let
				    val tname = fmatch kw_name
				    val rname = getOpt (smatch kw_root, tname)
				    val tclass = smatch kw_class
				    val topts = matches kw_options
				    val lf = smatch kw_lineformat
				    val cpif =
					case smatch kw_cpif of
					    NONE => true
					  | SOME s =>
					    (case Bool.fromString s of
						 SOME x => x
					       | NONE => badspec kw_cpif)
				in
				    oneTarget (tname, rname, tclass, topts,
					       lf, cpif)
				end
			      | _ => err "unrecognized target option(s)"
		    end
		in
		    if name = kw_target then subopts opts
		    else badkw name
		end
	    fun rulefn () =
		({ cmfiles = [], smlfiles = [],
		   sources = [(p, { class = class, derived = derived })] },
		 case too of
		     SOME opts => map oneOpt opts
		   | NONE => let
			 val { base, ext } = OS.Path.splitBaseExt sname
			 val base =
			     case ext of
				 NONE => base
			       | SOME e => if e = "nw" then base else sname
			 fun exp e = let
			     val tname = OS.Path.joinBaseExt { base = base,
							       ext = SOME e }
			 in
			     oneTarget (tname, tname, NONE, NONE, NONE, true)
			 end
		     in
			 [exp "sig", exp "sml"]
		     end)
	in
	    context rulefn
	end
	fun sfx s =
	    registerClassifier (stdSfxClassifier { sfx = s, class = class })
    in
        val _ = registerClass (class, rule)
	val _ = sfx "nw"
	fun lineNumbering class = let
	    fun get () = StringMap.find (!lnr, class)
	    fun set NONE =
		((lnr := #1 (StringMap.remove (!lnr, class)))
		 handle LibBase.NotFound => ())
	      | set (SOME f) = lnr := StringMap.insert (!lnr, class, f)
	in
	    { get = get, set = set }
	end
    end
end
