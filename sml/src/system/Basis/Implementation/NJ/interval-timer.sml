(* interval-timer.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * An interface to system interval timers.
 *
 *)

local
    structure Int = IntImp
    structure Int32 = Int32Imp
in
structure IntervalTimer : INTERVAL_TIMER =
  struct

    fun cfun x = CInterface.c_function "SMLNJ-RunT" x

    val tick' : unit -> (Int32.int * int) = cfun "intervalTick"
    val setITimer : (Int32.int * int) option -> unit = cfun "setIntTimer"

    fun tick () = let val (s, us) = tick'()
	  in
	    PreBasis.TIME{seconds= Int32.toLarge s, uSeconds= Int.toLarge us}
	  end

    fun fromTimeOpt NONE = NONE
      | fromTimeOpt (SOME(PreBasis.TIME{seconds, uSeconds})) =
	  SOME(Int32.fromLarge seconds, Int.fromLarge uSeconds)

    fun setIntTimer timOpt = setITimer(fromTimeOpt timOpt)

  end
end


