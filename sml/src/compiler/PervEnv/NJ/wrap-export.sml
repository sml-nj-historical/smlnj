(* wrap-export.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the wrapper for standalone programs.
 *
 *)

structure WrapExport : sig

  (* We need the PAIR wrapper to make sure that the second argument will be fully
   * wrapped when it is passed to the run-time system.
   *)
    datatype ('a, 'b) pair = PAIR of 'a * 'b

    val wrap : ((string * string list) -> OS.Process.status)
	  -> (string, string list) pair -> 'a

  end = struct

    structure Process = OS_Process

  (* We need the PAIR wrapper to make sure that the second argument will be fully
   * wrapped when it is passed to the run-time system.
   *)
    datatype ('a, 'b) pair = PAIR of 'a * 'b

    fun wrap f (PAIR args) = (
	  CleanUp.clean CleanUp.AtInitFn;
	  Process.exit((f args) handle exn => Process.failure))

  end (* WrapExport *)


(*
 * $Log: wrap-export.sml,v $
 * Revision 1.2  1997/08/13 17:23:13  jhr
 *   Minor clean-up of exportFn code.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
