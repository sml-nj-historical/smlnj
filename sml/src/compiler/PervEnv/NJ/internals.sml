(* internals.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure (SMLofNJ.Internals) is a gathering place for internal
 * features that need to be exposed outside the boot directory.
 *)

structure Internals : INTERNALS =
  struct

    structure CleanUp = CleanUp
    structure ProfControl = ProfControl
    structure GC = GC

    val prHook = PrintHook.prHook

    val initSigTbl = InternalSignals.initSigTbl
    val clearSigTbl = InternalSignals.clearSigTbl
    val resetSigTbl = InternalSignals.resetSigTbl

    val resetTimers = InternalTimer.resetTimers

  end;


(*
 * $Log: internals.sml,v $
 * Revision 1.5  1998/01/15 20:07:04  jhr
 *   Reorganized timer initialization code.  New structure InternalTimer
 *   exposes resetTimers function, which is exported in SMLofNJ.Internals
 *   and is used in the new structure CleanTimer.
 *
 * Revision 1.4  1997/09/04  19:42:09  jhr
 *   Split the Signal module into an internal and extarnal view.  Added hooks for
 *   initializing the signal handler tables, for programs (like CML) that need to
 *   bypass the standard clean-up mechanism.
 *
 * Revision 1.3  1997/02/11  15:16:17  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.2  1997/01/29  14:51:34  jhr
 * Added hook to allow dynamic rebinding of top-level print function.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
