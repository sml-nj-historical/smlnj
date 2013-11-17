(* xml-parser-fn.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ http://www.smlnj.org)
 * All rights reserved.
 *)

signature XML_PARSER =
  sig

    structure XMLTree : XML_TREE

    fun parseFile : string -> XMLTree.file

  end

functor XMLParserFn (XT : XML_TREE) : XML_PARSER =
  struct

    structure XMLTree = XT
    structure XS = XT.Schema

    datatype content = XT.content

    type stack = (element * attribute list) list

    type state = {
	stk : stack,		(* stack of currently open elements *)
	content : content list,	(* parse content in reverse order *)
	preWS : string option	(* preceeding WS when we are not preserving whitespace *)
      }

    fun mergeWS (NONE, content) = content
      | mergeWS (SOME ws, XT.TEXT txt :: content) = XT.TEXT(txt ^ ws) :: content
      | mergeWS (SOME s, content) = XT.TEXT s :: content

    fun add ({stk, content, preWS}, name, ) =
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

    fun parseContent (tokStrm, state) = (case nextTok tokStrm
	   of (T.EOF, _) => if List.null stk
		then List.rev content
		else (* error: missing close tags *)
	    | (T.OPEN_START_TAG, tokStrm) => parseStartTag (tokStrm, state)
	    | (T.OPEN_END_TAG , tokStrm)=> parseEndTag (tokStrm, state)
	    | (T.OPEN_XML_TAG, tokStrm) => parseXMLDecl (tokStrm, state)
	    | (T.WS s, tokStrm) => parseContent (tokStrm, addWS(state, s))
	    | (T.TEXT s, tokStrm) => parseContent (tokStrm, addText(state, s))
	    | (T.CDATA s, tokStrm) => parseContent (tokStrm, addCData(state, s))
	    | (tok, _) => raise Fail"impossible: unexpected ", Token.toString tok)
	  (* end case *))

  (* parse ID Attributes (">" | "/>") *)
    and parseStartTag (tokStrm, state) = (case nextTok tokStrm
	   of (T.ID id, tokStrm) => (case XS.element id
		 of SOME elem => let
		      val (attrs, tokStrm) = parsAttributes tokStrm
		      in
			case (nextTok tokStrm
			 of (T.CLOSE_TAG, tokStrm) =>
			      parseContent (tokStrm, push(state, elem, attrs))
			  | (T.CLOSE_EMPTY_TAG, tokStrm) =>
			      endElement (tokStrm,
				add(state, XT.ELEMENT{name=elem, attrs=attrs, content=[]}))
			  | (tok, _) => (* error *)
			(* end case *)
		      end
		  | NONE => (* error: unrecognized element *)
		(* end case *))
	    | (tok, _) => (* error: unrecognized element *)
	  (* end case *))

  (* parse ID ">" *)
    and parseEndTag (tokStrm, state) = (case nextTok tokStrm
	   of (T.ID id, tokStrm) => (case XS.element id
		 of SOME elem => let
		      val (content, attrs, state) = pop (state, elem)
		      in
			endElement (tokStrm,
			  add(state, XT.ELEMENT{name=elem, attrs=attrs, content=content}))
		      end
		  | NONE => (* error: unrecognized element *)
		(* end case *))
	    | (tok, _) => (* error: unrecognized element *)
	  (* end case *))

  (* handle an end tag or empty element tag *)
    and endElement (tokStrm, state) = if emptyStack state
	  then state
	  else parseContent (tokStrm, state)

  (* parse (ID "=" LIT)* *)
    and parseAttributes (tokStrm, state) = let
	  fun parseAttr (tokStrm, attrs) = (case nextTok tokStrm
		 of (T.ID id, tokStrm) => (case nextTok tokStrm
		       of (T.SYM_EQ, tokStrm) => (case nextTok tokStrm
			     of (T.LIT v, tokStrm) =>
				  parseAttr (tokStrm, XS.attribute(id, v)::attrs)
			      | (tok, _) => (* error: expected value *)
			    (* end case *))
			| (tok, _) => (* expected "=" *)
		      (* end case *))
		  | _ => (tokStrm, List.rev attrs)
		(* end case *))
	    |
	  in
	    parseAttr (tokStrm, [])

  (* parse Attributes "?>" *)
    and parseXMLDecl (tokStrm, state) = let
	  val (attrs, tokStrm) = parseAttributes (tokStrm, state)
	  in
	    case nextTok tokStrm
	     of (T.CLOSE_XML_TAG, tokStrm) => (attrs, tokStrm)
	      | (tok, _) => (* error: unrecognized element *)
	    (* end case *)
	  end

  (* parse XMLDecl? Content *)
    and parseFile tokStrm = let
	  val state = initialState()
	  val (xmlDecl, tokStrm) = (case nextTok tokStrm
		 of (T.OPEN_XML_TAG, tokStrm) => parseXMLDecl tokStrm
		  | _ => ([], tokStrm)
		(* end case *))
	  fun parse tokStrm = (case nextTok tokStrm
		 of (T.EOF, _) => {xmlDecl = xmlDecl, content = TEXT ""}
		  | (T.OPEN_START_TAG, tokStrm) => let
		      val finalState = parseStartTag (tokStrm, content, stk)
		      in
			{xmlDecl = xmlDecl, content = ??}
		      end
		  | T.WS _ => parse tokStrm
		  | tok, _) => raise Fail"impossible: unexpected ", Token.toString tok)
		(* end case *))
	  in
	    parse tokStrm before close tokStrm
	  end

  end
