(* interval-timer.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * An interface to system interval timers.
 *
 *)

structure IntervalTimer : INTERVAL_TIMER =
  struct

    fun cfun x = CInterface.c_function "SMLNJ-RunT" x

    val tick' : unit -> (Int32.int * int) = cfun "intervalTick"
    val setITimer : (Int32.int * int) option -> unit = cfun "setIntTimer"

    fun tick () = let val (s, us) = tick'()
	  in
	    PreBasis.TIME{sec= Int32.toLarge s, usec= Int.toLarge us}
	  end

    fun fromTimeOpt NONE = NONE
      | fromTimeOpt (SOME(PreBasis.TIME{sec, usec})) =
	  SOME(Int32.fromLarge sec, Int.fromLarge usec)

    fun setIntTimer timOpt = setITimer(fromTimeOpt timOpt)

  end;


(*
 * $Log: interval-timer.sml,v $
 * Revision 1.2  1997/07/31 17:25:20  jhr
 *   We are now using 32-bit ints to represent the seconds portion of a
 *   time value.  This was required to handle the change in the type of
 *   Time.{to,from}{Seconds,Milliseconds,Microseconds}.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
