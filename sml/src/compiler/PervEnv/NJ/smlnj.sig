(* smlnj.sig
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
 * $Log: smlnj.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
