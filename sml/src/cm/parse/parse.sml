(*
 * Parser for CM description files.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature PARSE = sig
    val parse :
	GeneralParams.param -> bool option ->
	AbsPath.t -> (CMSemant.group * GeneralParams.info) option
end

functor ParseFn (structure Stabilize: STABILIZE) :> PARSE = struct

    val lookAhead = 30

    structure S = GenericVC.Source
    structure EM = GenericVC.ErrorMsg
    structure SM = GenericVC.SourceMap

    structure CMLrVals = CMLrValsFun (structure Token = LrParser.Token)
    structure CMLex = CMLexFun (structure Tokens = CMLrVals.Tokens)
    structure CMParse =
	JoinWithArg (structure ParserData = CMLrVals.ParserData
		     structure Lex = CMLex
		     structure LrParser = LrParser)

    fun parse param stabflag group = let

	val stabthis = isSome stabflag
	val staball = stabflag = SOME true

	val groupreg = GroupReg.new ()
	val errcons = EM.defaultConsumer ()
	val ginfo = { param = param, groupreg = groupreg, errcons = errcons }

	(* The "group cache" -- we store "group options";  having
	 * NONE registered for a group means that a previous attempt
	 * to parse it had failed. *)
	val gc = ref (AbsPathMap.empty: CMSemant.group option AbsPathMap.map)

	fun mparse (group, groupstack, pErrFlag, stabthis) =
	    case AbsPathMap.find (!gc, group) of
		SOME g => g
	      | NONE => let
		    val g = parse' (group, groupstack, pErrFlag, stabthis)
		in
		    gc := AbsPathMap.insert (!gc, group, g);
		    g
		end

	and parse' (group, groupstack, pErrFlag, stabthis) = let
	    (* checking for cycles among groups and printing them nicely *)
	    fun findCycle ([], _) = []
	      | findCycle ((h as (g, (s, p1, p2))) :: t, cyc) =
		if AbsPath.compare (g, group) = EQUAL then rev (h :: cyc)
		else findCycle (t, h :: cyc)
	    fun report ((g, (s, p1, p2)), hist) = let
		fun pphist pps = let
		    fun loop (_, []) = ()
		      | loop (g0, (g, (s, p1, p2)) :: t) = let
			    val s = EM.matchErrorString s (p1, p2)
			in
			    PrettyPrint.add_string pps s;
			    PrettyPrint.add_string pps ": importing ";
			    PrettyPrint.add_string pps (AbsPath.spec g0);
			    PrettyPrint.add_newline pps;
			    loop (g, t)
			end
		in
		    PrettyPrint.add_newline pps;
		    loop (g, hist)
		end
	    in
		EM.error s (p1, p2) EM.COMPLAIN
		           ("group hierarchy forms a cycle with " ^
			    AbsPath.spec group)
			   pphist
	    end

	    fun getStable gpath = let
		val loadStable =
		    Stabilize.loadStable (ginfo, getStable, pErrFlag)
	    in
		case AbsPathMap.find (!gc, gpath) of
		    SOME (x as SOME _) => x
		  | SOME NONE => NONE
		  | NONE =>
			(case loadStable gpath of
			     NONE => NONE
			   | x as SOME _ =>
				 (gc := AbsPathMap.insert (!gc, gpath, x);
				  x))
	    end

	    fun stabilize g =
		Stabilize.stabilize ginfo { group = g, anyerrors = pErrFlag }

	    (* normal processing -- used when there is no cycle to report *)
	    fun normal_processing () = let
		val currentDir = AbsPath.dir group
		val context = AbsPath.relativeContext (AbsPath.dir group)
		val filename = AbsPath.name group
		val _ = Say.vsay ["[scanning ", filename, "]\n"]
		val stream = TextIO.openIn filename
		val source = S.newSource (filename, 1, stream, false, errcons)
		val sourceMap = #sourceMap source
		val _ = GroupReg.register groupreg (group, source)

		(* We can hard-wire the source into this
		 * error function because the function is only for
		 * immediate use and doesn't get stored into persistent
		 * data structures. *)
		fun error r m =
		    EM.error source r EM.COMPLAIN m EM.nullErrorBody

		(* recParse returns a group (not an option).
		 * This function is used to parse aliases and sub-groups.
		 * Errors are propagated by explicitly setting the
		 * "anyErrors" flag of the parent group. *)
		fun recParse (p1, p2) p = let
		    val groupstack' = (group, (source, p1, p2)) :: groupstack
		    val myErrorFlag = #anyErrors source
		in
		    case mparse (p, groupstack', myErrorFlag, staball) of
			NONE => (myErrorFlag := true;
				 CMSemant.emptyGroup group)
		      | SOME res => res
		end
	        handle exn as IO.Io _ =>
		    (error (p1, p2) (General.exnMessage exn);
		     CMSemant.emptyGroup group)

		fun doMember (p, p1, p2, c) =
		    CMSemant.member (ginfo, recParse (p1, p2))
		                    { sourcepath = p, class = c,
				      group = (group, (p1, p2)) }

		(* Build the argument for the lexer; the lexer's local
		 * state is encapsulated here to make sure the parser
		 * is re-entrant. *)
		val lexarg = let
		    (* local state *)
		    val depth = ref 0
		    val curstring = ref []
		    val startpos = ref 0
		    val instring = ref false
		    (* handling comments *)
		    fun enterC () = depth := !depth + 1
		    fun leaveC () = let
			val d = !depth - 1
		    in
			depth := d;
			d = 0
		    end
		    (* handling strings *)
		    fun newS pos =
			(instring := true; curstring := []; startpos := pos)
		    fun addS c = curstring := c :: !curstring
		    fun addSC (s, offs) =
			addS (chr (ord (String.sub (s, 2)) - offs))
		    fun addSN (s, pos) = let
			val ns = substring (s, 1, 3)
			val n = Int.fromString ns
		    in
			addS (chr (valOf n))
			handle _ =>
			    error (pos, pos + size s)
			          ("illegal decimal char spec: " ^ ns)
		    end
		    fun getS (pos, tok) =
			(instring := false;
			 tok (implode (rev (!curstring)), !startpos, pos))
		    (* handling EOF *)
		    fun handleEof () = let
			val pos = SM.lastChange sourceMap
		    in
			if !depth > 0 then
			    error (pos, pos)
			          "unexpected end of input in comment"
			else if !instring then
			    error (pos, pos)
			          "unexpected end of input in string"
			else ();
			pos
		    end
		    (* handling line breaks *)
		    fun newline pos = SM.newline sourceMap pos
		    (* handling #line directives *)
		    fun sync (p, t) = let
			fun sep c = c = #"#" orelse Char.isSpace c
			fun cvt s = getOpt (Int.fromString s, 0)
			fun r (line, col, file) = SM.resynch sourceMap
			    (p, { fileName = file, line = line, column = col })
		    in
			case String.tokens sep t of
			    [_, line] =>
				r (cvt line, NONE, NONE)
			  | [_, line, file] =>
				r (cvt line, NONE, SOME file)
			  | [_, line, col, file] =>
				r (cvt line, SOME (cvt col), SOME file)
			  | _ => error (p, p + size t)
				"illegal #line directive"
		    end
		in
		    { enterC = enterC,
		      leaveC = leaveC,
		      newS = newS,
		      addS = addS,
		      addSC = addSC,
		      addSN = addSN,
		      getS = getS,
		      handleEof = handleEof,
		      newline = newline,
		      error = error,
		      sync = sync}
		end

		fun inputc k = TextIO.input stream

		val lexer = CMLex.makeLexer inputc lexarg
		val tokenStream = LrParser.Stream.streamify lexer
		val (parseResult, _) =
		    CMParse.parse (lookAhead, tokenStream,
				   fn (s,p1,p2) => error (p1, p2) s,
				   (group, context, error, recParse,
				    doMember, ginfo))
	    in
		TextIO.closeIn stream;
		if !(#anyErrors source) then NONE
		else if stabthis then stabilize parseResult
		else SOME parseResult
	    end
            handle LrParser.ParseError => NONE
	in
	    case findCycle (groupstack, []) of
		h :: t => (report (h, t); NONE)
	      | [] =>
		    (case getStable group of
			 NONE => normal_processing ()
		       | SOME g => SOME g)
	end
    in
	case mparse (group, [], ref false, stabthis) of
	    NONE => NONE
	  | SOME g =>
		if CheckSharing.check (g, ginfo) then
		    (SmlInfo.forgetAllBut (Reachable.reachable g);
		     SOME (g, ginfo))
		else NONE
    end
end
