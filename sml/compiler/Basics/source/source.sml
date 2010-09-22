(* source.sml *)
(* COPYRIGHT (c) 1996 Bell Laboratories. *)

structure Source : SOURCE =
struct

  type inputSource = {
        sourceMap: SourceMap.sourcemap,
        fileOpened: string,
        interactive: bool,
        sourceStream: TextIO.instream,
        anyErrors: bool ref,
        errConsumer: PrettyPrintNew.device
      }

(* -- inactive diagnostic printing
  fun say (msg : string) = Control_Print.say msg
*)

  val lexer_initial_position = 1
  (* position of first input char according to lexer. 
   * A long-standing bug in ml-lex caused this to be 2. *)

  fun newSource(fileName, lineNum, sourceStream, interactive, errConsumer) =
      {sourceMap=SourceMap.newmap(lexer_initial_position, fileName, lineNum),
       sourceStream=sourceStream,interactive=interactive,fileOpened=fileName,
       errConsumer=errConsumer,anyErrors=ref false}

  fun closeSource ({interactive=true, ...} : inputSource) = ()
    | closeSource ({sourceStream, ...}) = (
        (* app say ["[closing ", (Pathnames.trim fileName), "]\n"];*)
        TextIO.closeIn sourceStream handle IO.Io _ => ())

  fun filepos ({sourceMap,...}: inputSource) pos = SourceMap.filepos sourceMap pos

end (* structure Source *)
