(* ______________________________________________________________________
   html4-parser.sml

   Defines the HTML4Parser structure, which defunctorizes the
   automatically generated parser, defines an additional set of
   utilities for working with the parser.

   ______________________________________________________________________ *)

structure HTML4Parser = struct

structure H4 = HTML4

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

fun fromParseTree pt =
    SOME (H4.HTML { version = NONE, head = [], content = H4.BODY([],[]) })

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
