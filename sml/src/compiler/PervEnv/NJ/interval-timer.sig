(* interval-timer.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * An interface to system interval timers.
 *
 *)

signature INTERVAL_TIMER =
  sig

    val tick : unit -> Time.time
	(* the minimum interval that the interval timers support *)

    val setIntTimer : Time.time option -> unit
	(* set the interval timer; NONE means to disable the timer. *)

  end;

(*
 * $Log: interval-timer.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
