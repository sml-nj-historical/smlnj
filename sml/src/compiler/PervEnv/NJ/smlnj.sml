(* smlnj.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure SMLofNJ (* : SML_OF_NJ *) =
  struct

  (* command-line arguments *)
    val getCmdName : unit -> string =
	  CInterface.c_function "SMLNJ-RunT" "cmdName"
    val getArgs : unit -> string list =
	  CInterface.c_function "SMLNJ-RunT" "argv"
    val getAllArgs : unit -> string list =
	  CInterface.c_function "SMLNJ-RunT" "rawArgv"

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


(*
 * $Log: smlnj.sml,v $
 * Revision 1.4  1997/09/12 18:00:57  jhr
 *   Changes the definition of the 'a frag datatype so that it is the same as
 *   the primitive type.
 *
 * Revision 1.3  1997/06/02  19:15:48  jhr
 *   Added getCmdName function.
 *
 * Revision 1.2  1997/03/03  17:10:42  george
 * moved callcc related functions to SMLofNJ.Cont
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
