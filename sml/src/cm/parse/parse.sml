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

    (* The error function must look for the source using the GroupReg
     * group register because the source must not be hard-wired into it
     * (via closure creation).  The reason for this is that the error
     * function will get cached in SmlInfo.info but the source will change
     * when one re-runs the analysis. *)

    fun parse primconf group = let

	val groupreg = GroupReg.new ()
	val fnpolicy = FilenamePolicy.default
	val params = { groupreg = groupreg,
		       fnpolicy = fnpolicy,
		       primconf = primconf }

	fun parse' (group, groupstack) = let

	    val currentDir = AbsPath.dir group
	    val context = AbsPath.relativeContext (AbsPath.dir group)
	    val filename = AbsPath.name group
	    val _ = Say.vsay (concat ["[scanning ", filename, "]\n"])
	    val stream = TextIO.openIn filename
	    val errcons =
		{ linewidth = !P.linewidth, flush = P.flush, consumer = P.say }
	    val source = S.newSource (filename, 1, stream, false, errcons)
	    val sourceMap = #sourceMap source
	    val _ = GroupReg.register groupreg (group, source)
	    fun error' region m b = let
		val src = GroupReg.lookup groupreg group
	    in
		EM.error src region EM.COMPLAIN m b
	    end
	    fun error region m = error' region m EM.nullErrorBody

	    (* recParse returns a group (not an option)
	     * and re-raises LrParser.ParseError.
	     * This exception will be handled by the surrounding
	     * call to parse.
	     * This function is used to parse aliases and sub-groups. *)
	    fun recParse (p1, p2) p =
		(case parse' (p, (group, (source, p1, p2)) :: groupstack) of
		     NONE => (#anyErrors source := true;
			      CMSemant.emptyGroup group)
		   | SOME res => res)
		     handle exn as IO.Io _ =>
			 (error (p1, p2) (General.exnMessage exn);
			  CMSemant.emptyGroup group)

	    fun doMember (p, p1, p2, c, e) =
		CMSemant.member (params, recParse (p1, p2))
		                { sourcepath = p,
				  group = group,
				  class = c,
				  error = e }

	    (* checking for cycles among groups and printing them nicely *)
	    val _ = let
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
			 pphist;
		    raise LrParser.ParseError
		end
	    in
		case findCycle (groupstack, []) of
		    [] => ()
		  | h :: t => report (h, t)
	    end

	    val lexarg = let
		(* local state *)
		val depth = ref 0
		val curstring = ref []
		val startpos = ref 0
		val instring = ref NONE
		(* handling comments *)
		fun enterC () = depth := !depth + 1
		fun leaveC () = let val d = !depth - 1 in depth := d; d = 0 end
		(* handling strings *)
		fun newS (pos, kind) =
		    (instring := SOME kind;
		     curstring := [];
		     startpos := pos)
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
		    (instring := NONE;
		     tok (implode (rev (!curstring)), !startpos, pos))
		(* handling EOF *)
		fun handleEof () = let
		    val pos = SM.lastChange sourceMap
		in
		    if !depth > 0 then
			error (pos, pos) "unexpected end of input in comment"
		    else if isSome (!instring) then
			error (pos, pos)
			    ("unexpected end of input in " ^ valOf (!instring))
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
			       (group, context, error', error, recParse,
				doMember))
	in
	    TextIO.closeIn stream;
	    if !(#anyErrors source) then NONE
	    else SOME parseResult
	end
        handle LrParser.ParseError => NONE
    in
	parse' (group, [])
    end
end
