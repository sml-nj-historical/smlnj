signature CMPARSE = sig
    val parse : AbsPath.t -> CMSemant.group option
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

    fun parse filename = let
	val currentDir = AbsPath.dir filename
	val filename = AbsPath.name filename
	val stream = TextIO.openIn filename
	val errcons =
	    { linewidth = !P.linewidth, flush = P.flush, consumer = P.say }
	val source = S.newSource (filename, 1, stream, false, errcons)
	val sourceMap = #sourceMap source
	fun error region m =
	    EM.error source region EM.COMPLAIN m EM.nullErrorBody

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
	    fun addSC (s, offs) = addS (chr (ord (String.sub (s, 2)) - offs))
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

	fun inputc k =
	    TextIO.input stream

	val lexer = CMLex.makeLexer inputc lexarg
	val tokenStream = LrParser.Stream.streamify lexer
	val (parseResult, _) =
	    CMParse.parse (lookAhead, tokenStream,
			   fn (s,p1,p2) => error (p1, p2) s,
			   (currentDir, error))
    in
	TextIO.closeIn stream;
	SOME parseResult
    end handle LrParser.ParseError => NONE
end
