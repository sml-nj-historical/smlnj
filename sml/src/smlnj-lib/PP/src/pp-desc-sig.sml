(* pp-desc-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * This interface provides a declarative way to specify pretty-printing.
 *)

signature PP_DESC =
  sig
    type box

    type token
	(* tokens are an abstraction of strings (allowing for different
	 * widths and style information).
	 *)
    type style

    datatype indent
      = Abs of int		(* indent relative to outer indentation *)
      | Rel of int		(* indent relative to start of box *)

    val hBox    : box list -> box
    val vBox    : (indent * box list) -> box
    val hvBox   : (indent * box list) -> box
    val hovBox  : (indent * box list) -> box
    val box     : (indent * box list) -> box

    val token   : token -> box
    val string  : string -> box

    val style   : (style * box list) -> box

    val break   : {nsp : int, offset : int} -> box
    val space   : int -> box
	(* space n == break{nsp=n, offset=0} *)
    val cut     : box
	(* cut == break{nsp=0, offset=0} *)
    val newline : box

    val onNewline : box -> box
	(* the box is emitted iff it is preceeded by a newline *)

  end

