(* clean-timer.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *)

structure CleanTimer :> sig end =
  struct

    val _ = CleanUp.addCleaner (
	  "InitTimers",
	  [CleanUp.AtInit, CleanUp.AtInitFn],
	  fn _ => InternalTimer.resetTimers())

  end;

(*
 * $Log: clean-timer.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:04  george
 * Version 110.5
 *
 *)
