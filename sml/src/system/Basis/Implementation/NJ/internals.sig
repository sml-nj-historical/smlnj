(* internals.sig
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure (SMLofNJ.Internals) is a gathering place for internal
 * features that need to be exposed outside the boot directory.
 *)

signature INTERNALS =
  sig

    structure CleanUp : CLEAN_UP
    structure ProfControl : PROF_CONTROL
    structure GC : GC

    val prHook : (string -> unit) ref
	(* this hook can be used to change the top-level print function *)

  (* Routines for managing the internal signal handler tables.  These are
   * for programs that must otherwise bypass the standard initialization
   * mechanisms.
   *)
    val initSigTbl : unit -> unit
    val clearSigTbl : unit -> unit
    val resetSigTbl : unit -> unit

  (* reset the total real and CPU time timers *)
    val resetTimers : unit -> unit

  end;


