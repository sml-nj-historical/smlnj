(* prof-control-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure implements the interface to the run-time system's profiling
 * support library.  It is not meant for general use.
 *
 *)

signature PROF_CONTROL =
  sig

  (* get the timer count array *)
    val getTimeArray : unit -> int array

    val profMode : bool ref	(* controls profile instrumentation *)
    val current : int ref

  (* turn on/off profile signals.  These functions set/clear the profMode
   * flag.
   *)
    val profileOn  : unit -> unit
    val profileOff : unit -> unit

    val getTimingMode : unit -> bool

  (* get the time quantum in microseconds *)
    val getQuantum : unit -> int

    datatype compunit = UNIT of {
	base: int,
	size: int,
	counts: int Array.array,
	names: string
      }
			   
    val runTimeIndex : int
    val minorGCIndex : int
    val majorGCIndex : int
    val otherIndex : int
    val compileIndex : int
    val numPredefIndices : int

    val units : compunit list ref

    val reset : unit -> unit

  (* space profiling hooks *)
    val spaceProfiling : bool ref
    val spaceProfRegister :
	  (Unsafe.Object.object * string -> Unsafe.Object.object) ref

  end;

(*
 * $Log: prof-control-sig.sml,v $
 * Revision 1.3  1997/09/22 19:51:04  jhr
 *   Changed Profiling API to use separate compiler and timer modes.
 *
 * Revision 1.2  1997/06/30  19:36:29  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
