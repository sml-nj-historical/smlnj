(* source.sig
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *)

signature SOURCE =
  sig
    type inputSource = {
        sourceMap: SourceMap.sourcemap,
        fileOpened: string,
        interactive: bool,
        sourceStream: TextIO.instream, 
        content: string option ref,
        anyErrors: bool ref,
        errConsumer: PrettyPrintNew.device
      }

    val newSource : (string * TextIO.instream * bool * PrettyPrintNew.device)
          -> inputSource

    val closeSource: inputSource -> unit

    val filepos: inputSource -> SourceMap.charpos -> SourceMap.sourceloc
    (* simply calls SourceMap.filepos on the sourceMap component of inputSource,
     * provided for convenience. *)

    val getContent : inputSource -> string option

    val regionContent : inputSource * SourceMap.region ->
			(string * SourceMap.region * int) option
  end

(*
The fileOpened field contains the name of the file that was opened to
produce a particular inputSource.  It is used to derive related
file names (for example, see CompileF.codeopt and CompileF.parse
in build/compile.sml.). It is also used when we need to access the content
of the sourcefile for error messages (getContent).  This assumes that the
current directory remains stable if the file name is a relative path.

newSource takes as argument a file name, corresponding instream,
a boolean flag indicating whether the source is interactive (i.e. stdIn),
and a prettyPrint device.

Note: newSource is only called with line number = 1.
*)

