(* pp-stream-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * This interface provides a output stream interface to pretty printing.
 *)

signature PP_STREAM =
  sig
    type device
    type stream

    type token
	(* tokens are an abstraction of strings (allowing for different
	 * widths and style information).
	 *)
    type style

    datatype indent
      = Abs of int		(* indent relative to outer indentation *)
      | Rel of int		(* indent relative to start of box *)

    val openStream  : device -> stream
    val flushStream : stream -> unit
    val closeStream : stream -> unit
    val getDevice   : stream -> device

    val openHBox   : stream -> unit
    val openVBox   : stream -> indent -> unit
    val openHVBox  : stream -> indent -> unit
    val openHOVBox : stream -> indent -> unit
    val openBox    : stream -> indent -> unit
    val closeBox   : stream -> unit

    val token   : stream -> token -> unit
    val string  : stream -> string -> unit

    val pushStyle : (stream * style) -> unit
    val popStyle  : stream -> unit

    val break   : stream -> {nsp : int, offset : int} -> unit
    val space   : stream -> int -> unit
	(* space n == break{nsp=n, offset=0} *)
    val cut     : stream -> unit
	(* cut == break{nsp=0, offset=0} *)
    val newline : stream -> unit
    val nbSpace : stream -> int -> unit
	(* emits a nonbreakable space *)

    val onNewline : stream -> unit -> unit
	(* the command is executed iff it is preceeded by a newline *)

    val control : stream -> (device -> unit) -> unit

  (* pretty-print a PP description *)
    type pp_desc
    val description : stream -> pp_desc -> unit

  (* PP description constructors *)
    structure Desc : sig
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
	val cut     : pp_desc
	val newline : pp_desc
	val control : (device -> unit) -> pp_desc
      end

  end

