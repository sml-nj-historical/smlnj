(* internals-sig.sml
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


(*
 * $Log: internals-sig.sml,v $
 * Revision 1.5  1998/01/15 20:07:04  jhr
 *   Reorganized timer initialization code.  New structure InternalTimer
 *   exposes resetTimers function, which is exported in SMLofNJ.Internals
 *   and is used in the new structure CleanTimer.
 *
 * Revision 1.4  1997/09/04  19:42:08  jhr
 *   Split the Signal module into an internal and extarnal view.  Added hooks for
 *   initializing the signal handler tables, for programs (like CML) that need to
 *   bypass the standard clean-up mechanism.
 *
 * Revision 1.3  1997/02/11  15:16:16  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.2  1997/01/29  14:51:33  jhr
 * Added hook to allow dynamic rebinding of top-level print function.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
