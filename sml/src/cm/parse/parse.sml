(*
 * Parser for CM description files.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CMPARSE = sig
    val parse : Primitive.configuration -> AbsPath.t -> CMSemant.group option
end

structure CMParse :> CMPARSE = struct

    val lookAhead = 30

    structure S = GenericVC.Source
    structure EM = GenericVC.ErrorMsg
    structure SM = GenericVC.SourceMap
    structure P = GenericVC.Control.Print

    structure CMLrVals = CMLrValsFun (structure Token = LrParser.Token)
    structure CMLex = CMLexFun (structure Tokens = CMLrVals.Tokens)
    structure CMParse =
	JoinWithArg (structure ParserData = CMLrVals.ParserData
		     structure Lex = CMLex
		     structure LrParser = LrParser)

    fun parse primconf group = let

	val groupreg = GroupReg.new ()
	val fnpolicy = FilenamePolicy.default
	val params = { groupreg = groupreg,
		       fnpolicy = fnpolicy,
		       primconf = primconf,
		       keep_going = false }

	val gc = ref AbsPathMap.empty	(* the "group cache" *)

	fun mparse (group, groupstack) =
	    case AbsPathMap.find (!gc, group) of
		SOME g => g
	      | NONE => let
		    val g = parse' (group, groupstack)
		in
		    gc := AbsPathMap.insert (!gc, group, g);
		    g
		end

	and parse' (group, groupstack) = let
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

	    (* normal processing -- used when there is no cycle to report *)
	    fun normal_processing () = let
		val currentDir = AbsPath.dir group
		val context = AbsPath.relativeContext (AbsPath.dir group)
		val filename = AbsPath.name group
		val _ = Say.vsay (concat ["[scanning ", filename, "]\n"])
		val stream = TextIO.openIn filename
		val errcons = { linewidth = !P.linewidth,
			        flush = P.flush,
				consumer = P.say }
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
		in
		    case mparse (p, groupstack') of
			NONE => (#anyErrors source := true;
				 CMSemant.emptyGroup group)
		      | SOME res => res
		end
	        handle exn as IO.Io _ =>
		    (error (p1, p2) (General.exnMessage exn);
		     CMSemant.emptyGroup group)

		fun doMember (p, p1, p2, c) =
		    CMSemant.member (params, recParse (p1, p2))
		                    { sourcepath = p, class = c,
				      group = (group, (p1, p2)) }

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
		      error = error }
		end

		fun inputc k = TextIO.input stream

		val lexer = CMLex.makeLexer inputc lexarg
		val tokenStream = LrParser.Stream.streamify lexer
		val (parseResult, _) =
		    CMParse.parse (lookAhead, tokenStream,
				   fn (s,p1,p2) => error (p1, p2) s,
				   (group, context, error, recParse,
				    doMember, params))
	    in
		TextIO.closeIn stream;
		if !(#anyErrors source) then NONE
		else SOME parseResult
	    end
            handle LrParser.ParseError => NONE
	in
	    case findCycle (groupstack, []) of
		h :: t => (report (h, t); NONE)
	      | [] => normal_processing ()
	end

    in
	mparse (group, [])
    end
end
