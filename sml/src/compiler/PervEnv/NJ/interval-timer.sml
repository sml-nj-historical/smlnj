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
 * $Log$
 *)
