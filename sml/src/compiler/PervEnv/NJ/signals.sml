(* signals.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * These are the two basic interfaces to the run-time system signals support.
 * The interface covers the basic signals operations, plus a small collection
 * of standard signals that should be portable to non-UNIX systems.
 *
 *)

structure Signals :> SIGNALS =
  struct

    open InternalSignals

    val _ = let
	  open CleanUp
	  in
	  (* install cleaning actions *)
	    addCleaner ("Signals.exportFn", [AtExportFn], clearSigTbl);
	    addCleaner ("Signals.initFn", [AtInitFn], initSigTbl);
	    addCleaner ("Signals.init", [AtInit], resetSigTbl)
	  end

  end; (* Signals *)


(*
 * $Log: signals.sml,v $
 * Revision 1.4  1997/09/04 19:42:09  jhr
 *   Split the Signal module into an internal and extarnal view.  Added hooks for
 *   initializing the signal handler tables, for programs (like CML) that need to
 *   bypass the standard clean-up mechanism.
 *
 * Revision 1.3  1997/02/26  21:00:28  george
 *    Defined a new top level Option structure. All 'a option related
 *    functions have been moved out of General.
 *
 * Revision 1.2  1997/01/31  20:39:48  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
