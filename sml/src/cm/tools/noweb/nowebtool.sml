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

	val kw_subdir = "subdir"
	val kw_witness = "witness"
	val kw_target = "target"	(* "master" keyword *)

	val kw_name = "name"		(* sub-keywords... *)
	val kw_root = "root"
	val kw_class = "class"
	val kw_options = "options"
	val kw_lineformat = "lineformat"
	val kwl = [kw_name, kw_root, kw_class, kw_options, kw_lineformat]

	val dfl_subdir = "NW"

	fun err msg = raise ToolError { tool = tool, msg = msg }
	fun kwerr what kw = err (concat [what, " keyword `", kw, "'"])
	fun badkw kw = kwerr "unknown" kw
	fun misskw kw = kwerr "missing" kw
	fun badspec kw = kwerr "bad specification for " kw
	fun dup kw = kwerr "duplicate" kw

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
	    val p = srcpath (mkpath str)
	    val sname = nativeSpec p
	    val (sd, wn) =
		case too of
		    NONE => (NONE, NONE)
		  | SOME l => let
			fun loop ([], sd, wn) = (sd, wn)
			  | loop (STRING _ :: t, sd, wn) = loop (t, sd, wn)
			  | loop (SUBOPTS { name, opts = [STRING s] } :: t,
				  sd, wn) =
			    if name = kw_subdir then
				case sd of
				    NONE =>
				    loop (t, SOME (#mkpath s (#name s)), wn)
				  | SOME _ => dup kw_subdir
			    else if name = kw_witness then
				case wn of
				    NONE =>
				    loop (t, sd, SOME (#mkpath s (#name s)))
				  | SOME _ => dup kw_witness
			    else loop (t, sd, wn)
			  | loop (SUBOPTS { name, ... } :: t, sd, wn) =
			    if name = kw_witness orelse name = kw_subdir then
				badspec name
			    else loop (t, sd, wn)
		    in
			loop (l, NONE, NONE)
		    end
	    val subdir_pp =
		case sd of
		    SOME pp => pp
		  | NONE => mkNativePath dfl_subdir
	    val subdir = nativePre subdir_pp
	    fun inSubdir f =
		if OS.Path.isRelative f then OS.Path.concat (subdir, f)
		else f
	    val wname = Option.map (inSubdir o nativeSpec o srcpath) wn
	    val (cpif, outd, upd_wtn) =
		case wname of
		    NONE => (false,
			     fn tname => outdated tool ([tname], sname),
			     fn () => ())
		  | SOME wn => (true,
				fn tname => outdated' tool { src = sname,
							     tgt = tname,
							     wtn = wn },
				fn () => TextIO.closeOut (openTextOut wn))
	    fun oneTarget (tname, rname, tclass, topts, lf) = let
		val tname = inSubdir tname
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
		    makeDirs tname;
		    vsay ["[", cmd, "]\n"];
		    if OS.Process.system cmd = OS.Process.success then ()
		    else err cmd
		end
	    in
		if outd tname then runcmd () else ();
		{ name = tname, mkpath = mkNativePath,
		  class = tclass, opts = topts, derived = true }
	    end

	    fun oneTarget' tname =
		oneTarget (tname, tname, NONE, NONE, NONE)

	    fun simpleTarget { name, mkpath } =
		oneTarget' (nativeSpec (srcpath (mkpath name)))

	    fun oneOpt (STRING x, rest) = simpleTarget x :: rest
	      | oneOpt (SUBOPTS { name, opts }, rest) = let
		    fun subopts [STRING x] = simpleTarget x
		      | subopts opts = let
			    val { matches, restoptions } =
				parseOptions { tool = tool, keywords = kwl,
					       options = opts }
			    fun fmatch kw =
				case matches kw of
				    NONE => misskw kw
				  | SOME [STRING { name, mkpath }] =>
				    nativeSpec (srcpath (mkpath name))
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
				in
				    oneTarget (tname, rname, tclass, topts, lf)
				end
			      | _ => err "unrecognized target option(s)"
		    end
		in
		    if name = kw_target then subopts opts :: rest
		    else if name = kw_subdir orelse name = kw_witness then rest
		    else badkw name
		end
	    fun rulefn () =
		({ cmfiles = [], smlfiles = [],
		   sources = [(p, { class = class, derived = derived })] },
		 case too of
		     SOME opts => foldr oneOpt [] opts
		   | NONE => let
			 val { base, ext } = OS.Path.splitBaseExt sname
			 val base =
			     case ext of
				 NONE => base
			       | SOME e => if e = "nw" then base else sname
			 fun exp e =
			     oneTarget' (OS.Path.joinBaseExt
					     { base = base, ext = SOME e })
		     in
			 [exp "sig", exp "sml"]
		     end)
	in
	    context rulefn before upd_wtn ()
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
