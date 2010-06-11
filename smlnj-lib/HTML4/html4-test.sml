(* ______________________________________________________________________
   html4-test.sml
   ______________________________________________________________________ *)

structure Test = struct

structure HTML4Parser = HTML4ParseFn(HTML4Lexer)

fun ptToStr (HTML4Utils.Lf tok) = HTML4Tokens.toString tok
  | ptToStr (HTML4Utils.Nd (ntAtom, children)) =
    String.concat [Atom.toString ntAtom, "[",
                   String.concatWith ", " (List.map ptToStr children), "]"]

fun parseStream inStream =
    let
        val sourceMap = AntlrStreamPos.mkSourcemap ()
        val lex = HTML4Lexer.lex sourceMap
        val stream = HTML4Lexer.streamifyInstream inStream
        val (result, _, _) = HTML4Parser.parse lex stream
    in 
        result
    end

val parsetreeStreamToString =
    HTML4Utils.mkParsetreeStreamToString HTML4Tokens.toString

fun handleFile fileName =
    let
        val inStream = TextIO.openIn fileName
        val concrete_pt_opt = parseStream inStream
        val pt_visit_strm = 
            case concrete_pt_opt of
                SOME concrete_pt => HTML4Utils.parsetreeToVisitationStream
                                        concrete_pt
              | NONE => HTML4Utils.StreamNil
    in
        print (parsetreeStreamToString pt_visit_strm)
    end

fun main (_, args) = (List.app handleFile args; OS.Process.success)
    handle ex => OS.Process.failure

end

(* ______________________________________________________________________
   End of html4-test.sml
   ______________________________________________________________________ *)
