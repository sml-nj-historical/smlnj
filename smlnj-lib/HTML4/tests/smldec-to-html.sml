(* ______________________________________________________________________
   smldec-to-html.sml
   ______________________________________________________________________ *)

structure Test = struct

structure H4U = HTML4Utils

(* ____________________________________________________________ *)

val templateStream =
    let val instrm = TextIO.openIn "template.html"
        val template_pt_opt = HTML4.parseStream instrm
    in
        TextIO.closeIn instrm;
        case template_pt_opt of
            SOME (H4U.Nd(_, children)) =>
            H4U.stream_concatl (map H4U.parsetreeToVisitationStream children)
          | _ => H4U.StreamNil
    end handle ex => H4U.StreamNil

(* ____________________________________________________________ *)

fun outputHTMLParseStream (istrm, ostrm) =
    let fun visit (H4U.EnterNT _, indent) = String.concat [indent, " "]
          | visit (H4U.ExitNT _, indent) = String.extract(indent, 1, NONE)
          | visit (H4U.VisitT tok, indent) =
            (TextIO.output(ostrm, String.concat [indent, HTML4Tokens.toString
                                                             tok, "\n"]);
             indent)
        val _ = H4U.stream_foldl visit "" istrm
    in () end

(* ____________________________________________________________ *)

fun parseFile filename =
    let val stream = TextIO.openIn filename
        val source = Source.newSource(filename, 1, stream, false,
                                      ErrorMsg.defaultConsumer())
        val result = SmlFile.parse source
    in Source.closeSource source; result end

(* ____________________________________________________________ *)

fun handleFile filename =
    let val intree = parseFile filename
        fun filterTemplate strm = strm
        val outstream = TextIO.openOut (filename ^ ".html")
        val _ = outputHTMLParseStream(filterTemplate templateStream, outstream)
    in TextIO.closeOut outstream end

(* ____________________________________________________________
   Main routine.
 *)

fun main (_, args) = (List.app handleFile args; OS.Process.success)
    handle ex => OS.Process.failure

end

(* ______________________________________________________________________
   End of smldec-to-html.sml
   ______________________________________________________________________ *)
