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
 * $Log$
 *)
