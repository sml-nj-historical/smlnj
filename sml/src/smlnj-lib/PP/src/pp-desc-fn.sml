(* pp-desc-fn.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * This interface provides a declarative way to specify pretty-printing.
 *)

functor PPDescFn (S : PP_STREAM) : PP_DESC =
  struct

    type token = S.token
    type style = S.style

    datatype indent
      = Abs of int		(* indent relative to outer indentation *)
      | Rel of int		(* indent relative to start of box *)

    datatype box
      = HBox of box list
      | VBox of (indent * box list)
      | HVBox of (indent * box list)
      | HOVBox of (indent * box list)
      | Box of (indent * box list)
      | Token of token
      | String of string
      | Style of (style * box list)
      | Break of {nsp : int, offset : int}
      | Newline
      | OnNewline of box

    val hBox    = HBox
    val vBox    = VBox
    val hvBox   = HVBox
    val hovBox  = HOVBox
    val box     = Box
    val token   = Token
    val string  = String
    val style   = Style
    val break   = Break
    fun space n = Break{nsp = n, offset = 0}
    val cut     = Break{nsp = 0, offset = 0}
    val newline = Newline
    val onNewline = OnNewline

(* functions to pretty-print to streams and devices? *)

  end;

