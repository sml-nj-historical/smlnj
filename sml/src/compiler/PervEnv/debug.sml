(* debug.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure provides access to the run-time system debug output
 * stream.  It is useful for debugging the basis code, since it doesn't
 * depend on the SML I/O stack.
 *
 *)

structure Debug : sig

    val pr : string -> unit
    val dummy : string -> unit

  end = struct

    val pr : string -> unit = CInterface.c_function "SMLNJ-RunT" "debug"
    val dummy : string -> unit = CInterface.c_function "SMLNJ-RunT" "dummy"

  end


(*
 * $Log: debug.sml,v $
 * Revision 1.2  1997/10/01 14:51:13  jhr
 *   Added a hook for calling the dummy function.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
