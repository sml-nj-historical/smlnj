(* os-process.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The generic process control interface.
 *
 *)

signature OS_PROCESS =
  sig

    eqtype status

    val success   : status
    val failure   : status

    val system    : string -> status

    val atExit    : (unit -> unit) -> unit

    val exit      : status -> 'a
    val terminate : status -> 'a

    val getEnv : string -> string option

  end

(*
 * $Log: os-process.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
