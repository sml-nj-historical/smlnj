(* pp-desc-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * This interface provides a declarative way to specify pretty-printing.
 *)

signature PP_DESC =
  sig
    structure PPS : PP_STREAM

    type pp_desc = PPS.pp_desc
    type token = PPS.token
    type style = PPS.style
    type indent = PPS.indent

    val hBox    : pp_desc list -> pp_desc
    val vBox    : (indent * pp_desc list) -> pp_desc
    val hvBox   : (indent * pp_desc list) -> pp_desc
    val hovBox  : (indent * pp_desc list) -> pp_desc
    val box     : (indent * pp_desc list) -> pp_desc

    val token   : token -> pp_desc
    val string  : string -> pp_desc

    val style   : (style * pp_desc list) -> pp_desc

    val break   : {nsp : int, offset : int} -> pp_desc
    val space   : int -> pp_desc
	(* space n == break{nsp=n, offset=0} *)
    val cut     : pp_desc
	(* cut == break{nsp=0, offset=0} *)
    val newline : pp_desc

    val control : (PPS.device -> unit) -> pp_desc

  end

