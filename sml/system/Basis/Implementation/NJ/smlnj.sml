(* smlnj.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure SMLofNJ (* : SML_OF_NJ *) =
  struct

  (* command-line arguments *)
    val getCmdName : unit -> string = SMLBasis.cmdName
    val getArgs : unit -> string list = SMLBasis.cmdArgs
    val getAllArgs : unit -> string list = SMLNJRuntime.rawArgv
    val shiftArgs : unit -> unit = SMLNJRuntime.shiftArgv

(** How do we define this here???
    val use = Compiler.Interact.use_file
**)

(*
    datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a
*)
    datatype frag = datatype PrimTypes.frag

    val exnHistory : exn -> string list =
	   InlineT.cast(fn (_,_,hist: string list) => hist)

  end;
