(* os-process.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The implementation of the generic process control interface (OS.Process).
 * The OS dependencies are hidden in the run-time system support.
 *
 *)

structure OS_Process : OS_PROCESS =
  struct

    structure CU = CleanUp

    type status = OS.Process.status (* int *)

    val success = 0
    val failure = 1

    val system : string -> status = CInterface.c_function "SMLNJ-OS" "system"

    local
      val hooks = ref ([] : (unit -> unit) list)
      val _ = CU.addCleaner (
	    "OS.Process",
	    [CU.AtExit],
	    fn _ => List.app (fn f => (f ()) handle _ => ()) (! hooks))
    in
    fun atExit hook = hooks := hook :: !hooks
    end

    val terminate : status -> 'a = CInterface.c_function "SMLNJ-OS" "exit"
    fun exit sts = (CU.clean CU.AtExit; terminate sts)

    val getEnv : string -> string option = CInterface.c_function "SMLNJ-OS" "getEnv"

  end

(*
 * $Log: os-process.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:26  george
 *   Version 109.24
 *
 *)
