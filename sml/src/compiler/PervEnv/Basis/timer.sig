(* timer.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TIMER =
  sig

    type cpu_timer
    type real_timer

    val totalCPUTimer : unit -> cpu_timer
    val startCPUTimer : unit -> cpu_timer
    val checkCPUTimer : cpu_timer -> {
	    usr : Time.time, sys : Time.time, gc : Time.time
	  }

    val totalRealTimer : unit -> real_timer
    val startRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time

  end (* TIMER *)


(*
 * $Log: timer.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 *)
