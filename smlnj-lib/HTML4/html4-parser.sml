(* ______________________________________________________________________
   html4-parser.sml

   Defines the HTML4Parser structure, which defunctorizes the
   automatically generated parser, defines an additional set of
   utilities for working with the parser.

   ______________________________________________________________________ *)

structure HTML4Parser = struct

structure H4 = HTML4
structure H4U = HTML4Utils
structure H4T = HTML4Tokens
structure H4TU = HTML4TokenUtils
structure AtomMap = H4TU.AtomMap

local
    structure TheParser = HTML4ParseFn(HTML4Lexer)
in
open TheParser
end

fun parseStream inStream =
    let
        val sourceMap = AntlrStreamPos.mkSourcemap ()
        val lex = HTML4Lexer.lex sourceMap
        val stream = HTML4Lexer.streamifyInstream inStream
        val (result, _, _) = parse lex stream
    in
        result
    end

exception IllFormedHTMLParseStream of
          H4T.token H4U.parsevisitation H4U.stream * string option

val tokVisitationToString = H4U.visitationToString H4T.toString

val strVisitationToString = H4U.visitationToString (fn x : string => x)

val visitationSimilar = let
    fun tokSimilarToString (tok1, tokStr2) = (H4T.toString tok1) = tokStr2
in H4U.visitationSame tokSimilarToString end

fun expect expectedVisit pstrm =
    let val pstrmHd = H4U.stream_hd pstrm
            handle _ => H4U.VisitT H4T.EOF
        fun expectationError () =
            let val msg = "Expected " ^ (strVisitationToString expectedVisit) ^  ", got "
                          ^ (tokVisitationToString pstrmHd) ^ " instead."
            in IllFormedHTMLParseStream(pstrm, SOME msg) end
    in
        if visitationSimilar(pstrmHd, expectedVisit) then H4U.stream_tl pstrm
        else raise (expectationError())
    end

fun expectEnterNT nt = expect (H4U.EnterNT (Atom.atom nt))

fun expectExitNT nt = expect (H4U.ExitNT (Atom.atom nt))

fun expectVisitT tokStr = expect (H4U.VisitT tokStr)

fun expectEnterNTInDomain ntMap pstrm =
    let val pstrmHd = H4U.stream_hd pstrm
            handle _ => H4U.VisitT H4T.EOF
        fun expectationError () =
            let val msg = String.concat ["Expected entry of one of ",
                                         String.concatWith
                                             ", " (map (Atom.toString)
                                                       (AtomMap.listKeys ntMap)),
                                         "; got ", tokVisitationToString pstrmHd,
                                         " instead."]
            in IllFormedHTMLParseStream(pstrm, SOME msg) end
    in case pstrmHd
        of H4U.EnterNT ntAtom =>
           if AtomMap.inDomain (ntMap, ntAtom) then AtomMap.lookup (ntMap, ntAtom)
           else raise (expectationError ())
         | _ => raise (expectationError ())
    end

fun optional optVisit (strm as H4U.StreamCons(strmHd, _)) =
    if visitationSimilar(strmHd, optVisit) then (H4U.stream_tl strm, SOME strmHd)
    else (strm, NONE)
  | optional _ _ = (H4U.StreamNil, NONE)

fun optVisitTok tokName strm =
    case optional (H4U.VisitT tokName) strm
     of (strm', SOME (H4U.VisitT tok)) => (strm', SOME tok)
      | _ => (strm, NONE)

fun isEnterNT nt pstrm = (expectEnterNT nt pstrm; true)
    handle IllFormedHTMLParseStream _ => false

fun isExitNT nt pstrm = (expectExitNT nt pstrm; true)
    handle IllFormedHTMLParseStream _ => false

fun isVisitT tokStr pstrm = (expectVisitT tokStr pstrm; true)
    handle IllFormedHTMLParseStream _ => false

fun isEither (is1, is2) pstrm = (is1 pstrm) orelse (is2 pstrm)

fun streamSkipUntil _ H4U.StreamNil = H4U.StreamNil
  | streamSkipUntil pred (strm as H4U.StreamCons (strmHd, _)) =
    if pred strmHd then strm else streamSkipUntil pred (H4U.stream_tl strm)

fun streamSkipWhile pred = streamSkipUntil (fn strmHd => not (pred strmHd))

fun streamConsumeUntil consumer until strm =
    let fun streamConsumeUntil' strm' acc =
            if until strm' then (strm', rev acc)
            else let val (strm'', consumerVal) = consumer strm
                 in streamConsumeUntil' strm'' (consumerVal :: acc) end
    in streamConsumeUntil' strm [] end

fun tokIsSpace (H4T.PCDATA pcstr) =
    let fun loop nil = true
          | loop (ch :: rst) = if Char.isSpace ch then loop rst else false
    in loop (String.explode pcstr) end
  | tokIsSpace _ = false

fun visitationIsSpace (H4U.VisitT tok) = tokIsSpace tok
  | visitationIsSpace _ = false

val skipWhitespace = streamSkipWhile visitationIsSpace

fun tokIsCdata (H4T.PCDATA _) = true
  | tokIsCdata (H4T.ENTITY_REF _) = true
  | tokIsCdata (H4T.CHAR_REF _) = true
  | tokIsCdata (H4T.COMMENT _) = true
  | tokIsCdata _ = false

fun isNotCdata (H4U.StreamCons(H4U.VisitT tok, _)) = not (tokIsCdata tok)
  | isNotCdata _ = true

exception InvalidToken of H4T.token

fun tokToCdata (H4T.PCDATA str) = H4.PCDATA str
  | tokToCdata (H4T.ENTITY_REF ent) = H4.ENTITY ent
  | tokToCdata (H4T.CHAR_REF chr) = H4.CHAR chr
  | tokToCdata (H4T.COMMENT cmt) = H4.COMMENT cmt
  | tokToCdata tok = raise (InvalidToken tok)

(*+DEBUG*)
fun tokToString (H4T.DOCTYPE doctypeStr) = doctypeStr
  | tokToString (H4T.PCDATA dataStr) = ("PCDATA \"" ^ (String.toString dataStr) ^ "\"")
  | tokToString (H4T.COMMENT commentStr) = commentStr
  | tokToString tok = H4TU.tokToString tok

fun printVisitationStream strm =
    print ((H4U.visitationToString tokToString (H4U.stream_hd strm)) ^ "\n")

fun printIllFormedErr (IllFormedHTMLParseStream (strm, msgOpt)) =
    (print "Error in parse stream at: ";
     printVisitationStream strm;
     if isSome msgOpt then print ("Message: " ^ (valOf msgOpt) ^ "\n") else ())
  | printIllFormedErr exn = raise exn
(*-DEBUG*)

fun getAttrsFromStream (H4U.StreamCons (H4U.VisitT tok, _)) =
    (case H4TU.tokGetAttrs tok
      of SOME attrs => attrs
       | NONE => [])
  | getAttrsFromStream _ = []

fun html0aryFromParseStream tag ctor pstrm =
    let val pstrm1 = expectEnterNT tag pstrm
        val pstrm2 = expectVisitT ("START" ^ (String.map Char.toUpper tag)) pstrm1
        val attrs = getAttrsFromStream pstrm1
        val pstrm3 = expectExitNT tag (skipWhitespace pstrm2)
    in
        (pstrm3, SOME (ctor attrs))
    end

fun listOfOptsToList lst = map Option.valOf lst

fun htmlNaryFromParseStream tag ctor childFromParseStream pstrm =
    let val tagUpper = (String.map Char.toUpper tag)
        val endTag = "END" ^ tagUpper
        val pstrm1 = expectEnterNT tag pstrm
        val pstrm2 = expectVisitT ("START" ^ tagUpper) pstrm1
        val attrs = getAttrsFromStream pstrm1
        val (pstrm3, children) =
            streamConsumeUntil childFromParseStream (isVisitT endTag)
                               (skipWhitespace pstrm2)
        val pstrm4 = expectVisitT endTag pstrm3
        val pstrm5 = expectExitNT tag (skipWhitespace pstrm4)
    in (pstrm5, SOME (ctor (attrs, listOfOptsToList children))) end

type parseVisitStream = H4T.token H4U.parsevisitation H4U.stream

val headContentHandlerMap : (parseVisitStream ->
                             parseVisitStream * 
                             H4.head_content option) AtomMap.map ref = ref AtomMap.empty

val blockContentHandlerMap : (parseVisitStream ->
                              parseVisitStream *
                              H4.block option) AtomMap.map ref = ref AtomMap.empty

val inlineContentHandlerMap : (parseVisitStream ->
                               parseVisitStream *
                               H4.inline option) AtomMap.map ref = ref AtomMap.empty

fun htmlFromParseStream pstrm0 =
    let val pstrm1 = (skipWhitespace o (expectEnterNT "document")) pstrm0
        val (pstrm2, doctypeTokOpt) = optVisitTok "DOCTYPE" pstrm1
        val theVersion = (case doctypeTokOpt
                           of SOME (H4T.DOCTYPE doctype) => SOME doctype
                            | _ => NONE)
        val (pstrm3, starthtmlTokOpt) = optVisitTok "STARTHTML" (skipWhitespace pstrm2)
        val (pstrm4, headDataListOpt) = headFromParseStream (skipWhitespace pstrm3)
    in if not (isSome headDataListOpt) then (pstrm4, NONE)
       else
           let val (pstrm5, contentDataOpt) = bodyOrFramesetFromParseStream pstrm4
           in if not (isSome contentDataOpt) then (pstrm5, NONE)
              else
                  let val (pstrm6, _) = optVisitTok "ENDHTML" pstrm5
                      val pstrm7 = (skipWhitespace o (expectExitNT "document") o
                                    skipWhitespace) pstrm6
                  in
                      (pstrm7, SOME (H4.HTML { version = theVersion,
                                               head = [],
                                               content = valOf contentDataOpt } ))
                  end
           end
    end
and headFromParseStream pstrm0 =
    let val pstrm1 = (skipWhitespace o (expectEnterNT "head")) pstrm0
        val (pstrm2, startheadTokOpt) = optVisitTok "STARTHEAD" pstrm1
        val (pstrm3, children) =
            streamConsumeUntil headContentFromParseStream (isEither(isExitNT "head",
                                                                    isVisitT "ENDHEAD"))
                               (skipWhitespace pstrm2)
        val (pstrm4, _) = optVisitTok "ENDHEAD" pstrm3
        val pstrm5 = expectExitNT "head" (skipWhitespace pstrm4)
    in (pstrm5, SOME []) end
and headContentFromParseStream pstrm =
    let val pstrmHd = H4U.stream_hd pstrm
        fun expectationErr () =
            let val msg = String.concat ["Expected entry of one of title, base, script,",
                                         "style, meta, link, or object, got ",
                                         tokVisitationToString pstrmHd, " instead."]
            in IllFormedHTMLParseStream (pstrm, SOME msg) end
    in case pstrmHd
        of H4U.EnterNT ntAtom =>
           (case AtomMap.find (!headContentHandlerMap, ntAtom)
             of SOME handler => handler pstrm
              | NONE => raise (expectationErr ()))
         | _ => raise (expectationErr ())
    end
and bodyOrFramesetFromParseStream pstrm =
    let fun bodyFromParseStream pstrm0 =
            let val pstrm1 = expectEnterNT "body" pstrm0
                val (pstrm2, startbodyTokOpt) = optVisitTok "STARTBODY" pstrm1
                val attrs = if isSome startbodyTokOpt
                            then case H4TU.tokGetAttrs (valOf startbodyTokOpt)
                                  of SOME attrs => attrs
                                   | NONE => []
                            else []
                val (pstrm3, children) =
                    streamConsumeUntil blockOrScriptFromParseStream
                                       (isEither(isExitNT "body", isVisitT "ENDBODY"))
                                       (skipWhitespace pstrm2)
                val (pstrm4, _) = optVisitTok "ENDBODY" pstrm3
                val pstrm5 = expectExitNT "body" (skipWhitespace pstrm4)
            in (pstrm5, SOME (H4.BODY (attrs, listOfOptsToList children))) end
        fun bodyOrFrameset_FRAMESET_FromParseStream pstrm0 =
            let val (pstrm1, framesetOpt) = framesetFromParseStream pstrm0
            in
                (pstrm1,
                 case framesetOpt
                  of SOME frameset => SOME (H4.BodyOrFrameset_FRAMESET frameset)
                   | NONE => NONE)
            end
    in
        if isEnterNT "body" pstrm then bodyFromParseStream pstrm
        else bodyOrFrameset_FRAMESET_FromParseStream pstrm
    end
and framesetFromParseStream pstrm0 =
    let val pstrm1 = expectEnterNT "frameset" pstrm0
        val pstrm2 = expectVisitT "STARTFRAMESET" pstrm1
        val attrs = getAttrsFromStream pstrm1
        val (pstrm3, children) =
            streamConsumeUntil framesetOrFrameFromParseStream
                               (isEither(isVisitT "ENDFRAMESET",
                                         isEnterNT "noframes"))
                               (skipWhitespace pstrm2)
        val (pstrm4, noframesOpt) =
            if isEnterNT "noframes" pstrm3 then
                let val (pstrm4', noframesOpt') = noFramesFromParseStream pstrm3
                in (skipWhitespace pstrm4', noframesOpt') end
            else (pstrm3, NONE)
        val pstrm5 = expectVisitT "ENDFRAMESET" pstrm4
        val pstrm6 = expectExitNT "frameset" (skipWhitespace pstrm5)
    in
        (pstrm6, SOME (H4.FRAMESET (attrs, listOfOptsToList children, noframesOpt)))
    end
and framesetOrFrameFromParseStream pstrm0 =
    let fun cvtFrameset (SOME frameset) = SOME (H4.FramesetOrFrame_FRAMESET frameset)
          | cvtFrameset NONE = NONE
        val pstrm1 = skipWhitespace pstrm0
        val (pstrm2, result) =
            if isEnterNT "frameset" pstrm1
            then let val (pstrm', result') = framesetFromParseStream pstrm1
                 in (pstrm', cvtFrameset result') end
            else html0aryFromParseStream "frame" H4.FRAME pstrm1
    in (skipWhitespace pstrm2, result) end
and noFramesFromParseStream pstrm = (pstrm, NONE) (* XXX *)
and flowFromParseStream pstrm =
    let val pstrmHd = H4U.stream_hd pstrm
        fun cvtBlock (SOME block) = SOME (H4.Flow_BLOCK block)
          | cvtBlock NONE = NONE
        fun cvtInline (SOME inline) = SOME (H4.Flow_INLINE inline)
          | cvtInline NONE = NONE
        fun handleInline pstrm =
            let val (pstrm', result') = inlineFromParseStream pstrm
            in (pstrm', cvtInline result') end
    in case pstrmHd
        of H4U.EnterNT ntAtom =>
           if AtomMap.inDomain (!blockContentHandlerMap, ntAtom)
           then let val (pstrm', result') = blockFromParseStream pstrm
                in (pstrm', cvtBlock result') end
           else handleInline pstrm
         | _ => handleInline pstrm
    end
and blockFromParseStream pstrm =
    (expectEnterNTInDomain (!blockContentHandlerMap) pstrm) pstrm
and inlineFromParseStream pstrm =
    let val pstrmHd = H4U.stream_hd pstrm
    in case pstrmHd
        of H4U.VisitT tok => (H4U.stream_tl pstrm, NONE) (* XXX *)
         | _ => (expectEnterNTInDomain (!inlineContentHandlerMap) pstrm) pstrm
    end
and listItemFromParseStream pstrm = (pstrm, NONE)
and scriptFromParseStream pstrm = (pstrm, NONE)
and paramFromParseStream pstrm = (pstrm, NONE)
and legendFromParseStream pstrm = (pstrm, NONE)
and defTermOrDescFromParseStream pstrm = (pstrm, NONE)
and tableDataFromParseStream pstrm = (pstrm, NONE)
and trFromParseStream pstrm = (pstrm, NONE)
and thOrTdFromParseStream pstrm = (pstrm, NONE)
and optgrouporOptionFromParseStream pstrm = (pstrm, NONE)
and htmlOptionFromParseStream pstrm = (pstrm, NONE)
and flowOrParamFromParseStream pstrm = (pstrm, NONE)
and blockOrScriptFromParseStream pstrm = (pstrm, NONE)
and blockOrAreaFromParseStream pstrm = (pstrm, NONE)
and objectFromParseStream pstrm =
    htmlNaryFromParseStream "object" H4.Head_OBJECT flowOrParamFromParseStream pstrm
and headScriptFromParseStream pstrm =
    let val (pstrm', scriptOpt) = scriptFromParseStream pstrm
    in case scriptOpt of NONE => (pstrm', NONE)
                       | SOME script => (pstrm', SOME (H4.Head_SCRIPT script))
    end

fun cdataFromParseStream pstrm =
    if isNotCdata pstrm
    then raise (IllFormedHTMLParseStream(pstrm, SOME "Expected character data."))
    else
        let val pstrmHd = H4U.stream_hd pstrm
            val pstrmTl = H4U.stream_tl pstrm
        in case pstrmHd
            of H4U.VisitT tok => (pstrmTl, SOME (tokToCdata tok))
             | _ => (pstrmTl, NONE)
        end

val titleFromParseStream =
    htmlNaryFromParseStream "title" H4.Head_TITLE cdataFromParseStream
val baseFromParseStream = html0aryFromParseStream "base" H4.Head_BASE
val metaFromParseStream = html0aryFromParseStream "meta" H4.Head_META
val linkFromParseStream = html0aryFromParseStream "link" H4.Head_LINK
fun pFromParseStream pstrm = (pstrm, NONE)
fun h1FromParseStream pstrm = (pstrm, NONE)
fun h2FromParseStream pstrm = (pstrm, NONE)
fun h3FromParseStream pstrm = (pstrm, NONE)
fun h4FromParseStream pstrm = (pstrm, NONE)
fun h5FromParseStream pstrm = (pstrm, NONE)
fun h6FromParseStream pstrm = (pstrm, NONE)
fun ulFromParseStream pstrm = (pstrm, NONE)
fun olFromParseStream pstrm = (pstrm, NONE)
fun dirFromParseStream pstrm = (pstrm, NONE)
fun menuFromParseStream pstrm = (pstrm, NONE)
fun preFromParseStream pstrm = (pstrm, NONE)
fun dlFromParseStream pstrm = (pstrm, NONE)
fun divFromParseStream pstrm = (pstrm, NONE)
fun noscriptFromParseStream pstrm = (pstrm, NONE)
fun blockquoteFromParseStream pstrm = (pstrm, NONE)
fun formFromParseStream pstrm = (pstrm, NONE)
fun hrFromParseStream pstrm = (pstrm, NONE)
fun tableFromParseStream pstrm = (pstrm, NONE)
fun fieldsetFromParseStream pstrm = (pstrm, NONE)
fun addressFromParseStream pstrm = (pstrm, NONE)
fun centerFromParseStream pstrm = (pstrm, NONE)
fun isindexFromParseStream pstrm = (pstrm, NONE)
fun ttFromParseStream pstrm = (pstrm, NONE)
fun iFromParseStream pstrm = (pstrm, NONE)
fun bFromParseStream pstrm = (pstrm, NONE)
fun bigFromParseStream pstrm = (pstrm, NONE)
fun smallFromParseStream pstrm = (pstrm, NONE)
fun uFromParseStream pstrm = (pstrm, NONE)
fun sFromParseStream pstrm = (pstrm, NONE)
fun strikeFromParseStream pstrm = (pstrm, NONE)
fun emFromParseStream pstrm = (pstrm, NONE)
fun strongFromParseStream pstrm = (pstrm, NONE)
fun dfnFromParseStream pstrm = (pstrm, NONE)
fun codeFromParseStream pstrm = (pstrm, NONE)
fun sampFromParseStream pstrm = (pstrm, NONE)
fun kbdFromParseStream pstrm = (pstrm, NONE)
fun varFromParseStream pstrm = (pstrm, NONE)
fun citeFromParseStream pstrm = (pstrm, NONE)
fun abbrFromParseStream pstrm = (pstrm, NONE)
fun acronymFromParseStream pstrm = (pstrm, NONE)
fun aFromParseStream pstrm = (pstrm, NONE)
fun imgFromParseStream pstrm = (pstrm, NONE)
fun objectFromParseStream pstrm = (pstrm, NONE)
fun brFromParseStream pstrm = (pstrm, NONE)
fun inlineScriptFromParseStream pstrm = (pstrm, NONE)
fun mapFromParseStream pstrm = (pstrm, NONE)
fun qFromParseStream pstrm = (pstrm, NONE)
fun subFromParseStream pstrm = (pstrm, NONE)
fun supFromParseStream pstrm = (pstrm, NONE)
fun spanFromParseStream pstrm = (pstrm, NONE)
fun bdoFromParseStream pstrm = (pstrm, NONE)
fun appletFromParseStream pstrm = (pstrm, NONE)
fun basefontFromParseStream pstrm = (pstrm, NONE)
fun fontFromParseStream pstrm = (pstrm, NONE)
fun iframeFromParseStream pstrm = (pstrm, NONE)
fun inputFromParseStream pstrm = (pstrm, NONE)
fun selectFromParseStream pstrm = (pstrm, NONE)
fun textareaFromParseStream pstrm = (pstrm, NONE)
fun labelFromParseStream pstrm = (pstrm, NONE)
fun buttonFromParseStream pstrm = (pstrm, NONE)
fun inlineCdataFromParseStream pstrm = (pstrm, NONE)

val _ =
    (headContentHandlerMap := (foldl AtomMap.insert' AtomMap.empty
                                     [ (Atom.atom "title", titleFromParseStream),
                                       (Atom.atom "base", baseFromParseStream),
                                       (Atom.atom "script", headScriptFromParseStream),
                                       (Atom.atom "meta", metaFromParseStream),
                                       (Atom.atom "link", linkFromParseStream),
                                       (Atom.atom "object", objectFromParseStream) ]),
     blockContentHandlerMap := (foldl AtomMap.insert' AtomMap.empty
                                      [ (Atom.atom "p", pFromParseStream),
                                        (Atom.atom "h1", h1FromParseStream),
                                        (Atom.atom "h2", h2FromParseStream),
                                        (Atom.atom "h3", h3FromParseStream),
                                        (Atom.atom "h4", h4FromParseStream),
                                        (Atom.atom "h5", h5FromParseStream),
                                        (Atom.atom "h6", h6FromParseStream),
                                        (Atom.atom "ul", ulFromParseStream),
                                        (Atom.atom "ol", olFromParseStream),
                                        (Atom.atom "dir", dirFromParseStream),
                                        (Atom.atom "menu", menuFromParseStream),
                                        (Atom.atom "pre", preFromParseStream),
                                        (Atom.atom "dl", dlFromParseStream),
                                        (Atom.atom "div", divFromParseStream),
                                        (Atom.atom "noscript", noscriptFromParseStream),
                                        (Atom.atom "blockquote",
                                         blockquoteFromParseStream),
                                        (Atom.atom "form", formFromParseStream),
                                        (Atom.atom "hr", hrFromParseStream),
                                        (Atom.atom "table", tableFromParseStream),
                                        (Atom.atom "fieldset", fieldsetFromParseStream),
                                        (Atom.atom "address", addressFromParseStream),
                                        (Atom.atom "isindex", isindexFromParseStream),
                                        (Atom.atom "center", centerFromParseStream) ]),
     inlineContentHandlerMap := (foldl AtomMap.insert' AtomMap.empty
                                       [(Atom.atom "tt", ttFromParseStream),
                                        (Atom.atom "i", iFromParseStream),
                                        (Atom.atom "b", bFromParseStream),
                                        (Atom.atom "big", bigFromParseStream),
                                        (Atom.atom "small", smallFromParseStream),
                                        (Atom.atom "u", uFromParseStream),
                                        (Atom.atom "s", sFromParseStream),
                                        (Atom.atom "strike", strikeFromParseStream),
                                        (Atom.atom "em", emFromParseStream),
                                        (Atom.atom "strong", strongFromParseStream),
                                        (Atom.atom "dfn", dfnFromParseStream),
                                        (Atom.atom "code", codeFromParseStream),
                                        (Atom.atom "samp", sampFromParseStream),
                                        (Atom.atom "kbd", kbdFromParseStream),
                                        (Atom.atom "var", varFromParseStream),
                                        (Atom.atom "cite", citeFromParseStream),
                                        (Atom.atom "abbr", abbrFromParseStream),
                                        (Atom.atom "acronym", acronymFromParseStream),
                                        (Atom.atom "a", aFromParseStream),
                                        (Atom.atom "img", imgFromParseStream),
                                        (Atom.atom "object", objectFromParseStream),
                                        (Atom.atom "br", brFromParseStream),
                                        (Atom.atom "script",
                                         inlineScriptFromParseStream),
                                        (Atom.atom "map", mapFromParseStream),
                                        (Atom.atom "q", qFromParseStream),
                                        (Atom.atom "sub", subFromParseStream),
                                        (Atom.atom "sup", supFromParseStream),
                                        (Atom.atom "span", spanFromParseStream),
                                        (Atom.atom "bdo", bdoFromParseStream),
                                        (Atom.atom "applet", appletFromParseStream),
                                        (Atom.atom "basefont", basefontFromParseStream),
                                        (Atom.atom "font", fontFromParseStream),
                                        (Atom.atom "iframe", iframeFromParseStream),
                                        (Atom.atom "input", inputFromParseStream),
                                        (Atom.atom "select", selectFromParseStream),
                                        (Atom.atom "textarea", textareaFromParseStream),
                                        (Atom.atom "label", labelFromParseStream),
                                        (Atom.atom "button", buttonFromParseStream) ])
    )

fun fromParseTree pt =
    let val (_, result) = htmlFromParseStream (H4U.parsetreeToVisitationStream pt)
    in result end

fun fromString str = let
    val pt_opt = parseStream (TextIO.openString str)
in case pt_opt
    of NONE => NONE 
     | SOME pt => fromParseTree pt
end

end (* HTML4ParserUtils *)

(* ______________________________________________________________________
   End of html4-parser.sml
   ______________________________________________________________________ *)
