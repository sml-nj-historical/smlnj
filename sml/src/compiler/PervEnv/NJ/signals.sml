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
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
