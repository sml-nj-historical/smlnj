(* debug.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Debugging support for the CML core.
 *)

structure Debug : sig

    val sayDebug : string -> unit

  end = struct

    val sayDebug : string -> unit =
	  Unsafe.CInterface.c_function "SMLNJ-RunT" "debug"

  end

