(* smlnj-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature SML_OF_NJ =
  sig

    structure Cont : CONT
    structure IntervalTimer : INTERVAL_TIMER
    structure Internals : INTERNALS
    structure SysInfo : SYS_INFO
    structure Weak : WEAK

    val exportML : string -> bool
    val exportFn : (string * ((string * string list) -> OS.Process.status)) -> unit

  (* command-line arguments *)
    val getCmdName : unit -> string
    val getArgs    : unit -> string list
    val getAllArgs : unit -> string list

(** can't handle this yet **
    val use : string -> unit
**)

    datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a

    val exnHistory : exn -> string list

  end;


(*
 * $Log: smlnj-sig.sml,v $
 * Revision 1.6  1998/02/15 19:40:31  jhr
 *   Deleted SMLofNJ.Susp structure.
 *
 * Revision 1.5  1997/06/02 19:15:47  jhr
 *   Added getCmdName function.
 *
 * Revision 1.4  1997/04/10  14:35:47  dbm
 *   Changed return type of exportFn to unit.
 *
 * Revision 1.3  1997/03/03  17:10:41  george
 * moved callcc related functions to SMLofNJ.Cont
 *
 * Revision 1.2  1997/02/11  15:16:19  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
