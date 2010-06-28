(* ______________________________________________________________________
   html4-test.sml
   ______________________________________________________________________ *)

structure Test = struct

val parsetreeStreamToString =
    HTML4Utils.mkParsetreeStreamToString HTML4Tokens.toString

fun handleFile fileName =
    let
        val inStream = TextIO.openIn fileName
        val concrete_pt_opt = HTML4Parser.parseStream inStream
        val pt_visit_strm = 
            case concrete_pt_opt of
                SOME concrete_pt => HTML4Utils.parsetreeToVisitationStream
                                        concrete_pt
              | NONE => HTML4Utils.StreamNil
        val (_, htmlOpt) = HTML4Parser.htmlFromParseStream pt_visit_strm
            handle HTML4Parser.IllFormedHTMLParseStream (strm, SOME msg) =>
                   (HTML4Parser.printVisitationStream strm;
                    print (msg ^ "\n\n"); (strm, NONE))
        val outDoc = if isSome htmlOpt
                     then (HTML4Printer.toString (valOf htmlOpt)) ^ "\n\n"
                     else ""
    in
        print (parsetreeStreamToString pt_visit_strm);
        print ("\n\n" ^ outDoc)
    end

fun main (_, args) = (List.app handleFile args; OS.Process.success)
    handle ex => OS.Process.failure

end

(* ______________________________________________________________________
   End of html4-test.sml
   ______________________________________________________________________ *)
