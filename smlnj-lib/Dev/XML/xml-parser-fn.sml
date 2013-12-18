(* xml-parser-fn.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ http://www.smlnj.org)
 * All rights reserved.
 *)

signature XML_PARSER =
  sig

    structure XMLTree : XML_TREE

    val parseFile : string -> XMLTree.file

    exception ParseError of string

  end

functor XMLParserFn (XT : XML_TREE) : XML_PARSER =
  struct

    structure XMLTree = XT
    structure XS = XT.Schema

    datatype token = datatype XMLTokens.token

  (***** Error messages *****)

    exception ParseError of string

    datatype error_tag
      = S of string
      | ID of string
      | TK of token
      | E of XT.Schema.element

    fun error msg = let
	  fun cvt (S s, l) = s :: l
	    | cvt (ID id) = "\"" :: id :: "\"" :: l
	    | cvt (TK tok) = XMLTokens.toString tok :: l
	    | cvt (E elem) = XS.toString elem :: l
	  in
	    raise ParseError(String.concat(List.foldr cvt [] msg))
	  end

  (***** Token streams wrap the ML-ULex generated lexer *****
   *
   * We cache tokens to avoid rescanning the source.
   *)

    type lexer_state = XMLLexer.prestrm * XMLLexer.yystart_state

    datatype token_strm_rep
      = TOK of {tok : token, span : XMLLexer.span, more : token_strm}
      | MORE of {
	  state : lexer_state,
	  get : lexer_state -> token * XMLLexer.span * lexer_state
	}

    withtype token_strm = token_strm_rep ref

    fun newTokenStrm (initialState, lexFn) =
	  ref(MORE{state = initialState, get=lexFn})

    fun nextTok (ref(TOK{tok, span, more})) = (tok, span, more)
      | nextTok (strm as ref(MORE{state, get})) = let
	  val (tok, span, state) = get state
	  val more = ref(MORE{state=state, get=lexFn})
	  val rep = TOK{tok=tok, span=span, more=more}
	  in
	    strm := rep; (* cache lexer result *)
	    (tok, more)
	  end

    datatype content = datatype XT.content

  (***** Stack of open elements *****)

    type stack = (element * attribute list) list

  (****** Parser state *****)

    type state = {
	stk : stack,		(* stack of currently open elements *)
	content : content list,	(* parsed content in reverse order *)
	preWS : string option	(* preceeding WS when we are not preserving whitespace *)
      }

    fun mergeWS (NONE, content) = content
      | mergeWS (SOME ws, XT.TEXT txt :: content) = XT.TEXT(txt ^ ws) :: content
      | mergeWS (SOME s, content) = XT.TEXT s :: content

    fun add ({stk, content, preWS}, name, elem) =
	  {stk = stk, content = XT.ELEMENT elem :: mergeWS (preWS, content), preWS = NONE}

    fun addWS ({stk, content, preWS}, ws) = (case preWS
	   of SOME ws' => {stk = stk, content = content, preWS = SOME(ws' ^ ws)}
	    | NONE => {stk = stk, content = content, preWS = SOME ws}
	  (* end case *))

    fun addText ({stk, content, preWS}, txt) = let
	  val content = (case (preWS, content)
		 of (NONE, XT.TEXT txt' :: content) => XT.TEXT(txt' ^ txt) :: content
		  | (NONE, content) => XT.TEXT txt :: content
		  | (SOME ws, XT.TEXT txt :: content) => XT.TEXT(concat[txt', ws, txt] :: content)
		  | (SOME ws, content) => XT.TEXT(txt ^ ws) :: content
		(* end case *))
	  in
	    {stk = stk, content = content, preWS = NONE}
	  end

    fun addCData ({stk, content, preWS}, cdata) =
	  {stk = stk, content = XT.CDATA cdata :: mergeWS (preWS, content), preWS = NONE}

  (***** Parsing *****)

    fun parser (name, inStrm) = let
	  val srcMap = AntlrStreamPos.mkSourcemap' name
	  fun err (span, msg) =
		error(S "Error [" :: S(AntlrStreamPos.spanToString srcMap span) :: S "]: " :: msg)
	(* scan an element identifier *)
	  fun getElementId tokStrm = (case nextTok tokStrm
		 of (ID id, tokStrm) => (case XS.element id
		       of SOME elem => (elem, tokStrm)
			| NONE => err(span, [S "unrecognized element ", S id])
		      (* end case *))
		  | (tok, span, _) => err(span, [S "expected identifier, but found ", TK tok])
		(* end case *))
	(* parse the content of an element *)
	  fun parseContent (tokStrm, state) = (case nextTok tokStrm
		 of (EOF, span, _) => (case (#stk state)
		       of [] => List.rev content
			| (elem, _)::_ => err(span, [S "missing close ", E elem])
		      (* end case *))
		  | (OPEN_START_TAG, _, tokStrm) => parseStartTag (tokStrm, state)
		  | (OPEN_END_TAG, _, tokStrm)=> parseEndTag (tokStrm, state)
		  | (WS s, _, tokStrm) => parseContent (tokStrm, addWS(state, s))
		  | (TEXT s, _, tokStrm) => parseContent (tokStrm, addText(state, s))
		  | (COM s, _, tokStrm) => parseContent (tokStrm, addCom(state, s))
		  | (CDATA s, _, tokStrm) => parseContent (tokStrm, addCData(state, s))
		  | (tok, span, _) => err(span, [S "impossible: unexpected ", TK tok])
		(* end case *))
	(* expect: ID Attributes (">" | "/>") *)
	  and parseStartTag (tokStrm, state) = let
		val (elem, tokStrm) = getElementId tokStrm
		val (attrs, tokStrm) = parsAttributes tokStrm
		in
		  case (nextTok tokStrm)
		   of (CLOSE_TAG, _, tokStrm) =>
			parseContent (tokStrm, push(state, elem, attrs))
		    | (CLOSE_EMPTY_TAG, _, tokStrm) =>
			endElement (tokStrm,
			  add(state, XT.ELEMENT{name=elem, attrs=attrs, content=[]}))
		    | (tok, span, _) => err(span, [S "expected \">\" or \"/>\", but found ", TK tok])
		  (* end case *)
		end
	(* expect: ID ">" *)
	  and parseEndTag (tokStrm, state) = let
		val (elem, tokStrm) = getElementId tokStrm
		val (content, attrs, state) = pop (state, elem)
		in
		  endElement (tokStrm,
		    add(state, XT.ELEMENT{name=elem, attrs=attrs, content=content}))
		end
	(* handle an end tag or empty element tag *)
	  and endElement (tokStrm, state) = if emptyStack state
		then state
		else parseContent (tokStrm, state)
	(* expect: (ID "=" LIT)* *)
	  and parseAttributes (tokStrm, state) = let
		fun parseAttr (tokStrm, attrs) = (case nextTok tokStrm
		       of (ID id, _, tokStrm) => (case nextTok tokStrm
			     of (SYM_EQ, tokStrm) => (case nextTok tokStrm
				   of (LIT v, _, tokStrm) =>
					parseAttr (tokStrm, XS.attribute(id, v)::attrs)
				    | (tok, span, _) => err(span, [S "expected attribute value, but found ", TK tok])
				  (* end case *))
			      | (tok, span, _) => err(span, [S "expected \"=\", but found ", TK tok])
			    (* end case *))
			| _ => (tokStrm, List.rev attrs)
		      (* end case *))
		in
		  parseAttr (tokStrm, [])
		end
	(* expect: Attributes "?>" *)
	  and parseXMLDecl (tokStrm, state) = let
		val (attrs, tokStrm) = parseAttributes (tokStrm, state)
		in
		  case nextTok tokStrm
		   of (CLOSE_XML_TAG, _, tokStrm) => (attrs, tokStrm)
		    | (tok, span, _) => err(span, [S "expected \"?>\", but found ", TK tok])
		  (* end case *)
		end
	(* expect: ID (S ExternalID)? S? '>'
	 * where
	 *	ExternalID ::= 'SYSTEM' LIT
	 *	            |  'PUBLIC' LIT LIT
	 *)
	  fun parseDoctype (tokStrm, state) = raise Fail "FIXME"
	(* initialize the token stream *)
	  val tokStrm = newTokenStrm (
		XMLLexer.streamifyInstream inStrm,
		XMLLexer.lex srcMap (fn (pos, msg) => err((pos, pos), msg)))
	(* parse the XML Decl (if any) *)
	  val (xmlDecl, tokStrm) = let
		fun getXMLDecl tokStrm = (case nextTok tokStrm
		       of (OPEN_XML_TAG, _, tokStrm) => parseXMLDecl tokStrm
			| (WS _, _, tokStrm) => getXMLDecl tokStrm
			| (COM _, _, tokStrm) => getXMLDecl tokStrm
			| _ => (NONE, tokStrm)
		      (* end case *))
		in
		  getXMLDecl tokStrm
		end
	(* initial parser state *)
	  val state = initialState()
	  in
raise Fail "FIXME"
	  end (* parser *)

(*
	(* parse XMLDecl? Content *)
	  and parse tokStrm = let
		fun parse tokStrm = (case nextTok tokStrm
		       of (EOF, _) => {xmlDecl = xmlDecl, content = TEXT ""}
			| (OPEN_START_TAG, tokStrm) => let
			    val finalState = parseStartTag (tokStrm, content, stk)
			    in
			      {xmlDecl = xmlDecl, content = ??}
			    end
			| WS _ => parse tokStrm
			| tok, _) => err(?, [S "impossible: unexpected ", TK tok])
		      (* end case *))
		in
		  parse tokStrm before close tokStrm
		end
*)

    fun parseFile file = let
	  val inStrm = TextIO.openIn file
	  val tree = parser (file, inStrm)
		handle ex => (TextIO.closeIn inStrm; raise ex)
	  in
	    TextIO.closeIn inStrm;
	    tree
	  end

  end
