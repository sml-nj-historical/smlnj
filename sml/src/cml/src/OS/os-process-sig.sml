(* os-process-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The CML version of the generic process control interface.
 *)

signature CML_OS_PROCESS =
  sig

    eqtype status

    val success   : status
    val failure   : status

    val system    : string -> status
    val systemEvt : string -> status Event.event

    val atExit    : (unit -> unit) -> unit

    val exit      : status -> 'a
    val terminate : status -> 'a

    val getEnv : string -> string option

  end
