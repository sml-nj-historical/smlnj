(* profile.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * User interface for controling profiling.
 *
 *)

signature PROFILE =
  sig

  (* set/get the compilation mode *)
    val setProfMode : bool -> unit
    val getProfMode : unit -> bool

  (* set/get the profile timing mode *)
    val setTimingMode : bool -> unit
    val getTimingMode : unit -> bool

    val reset : unit -> unit
	(* reset profiling counts to zero.  This is done automatically, when
	 * the timing mode is enabled by setTimingMode.
	 *)

    val report : TextIO.outstream -> unit
	(* print profiling report to stream *)
    val reportAll : TextIO.outstream -> unit
	(* print profiling report to stream; DON'T suppress zero entries*)
    val reportData: unit -> {name: string, count: int, time: Time.time} list
	(* Return the unformatted data for a report *)

  end;

(*
 * $Log: profile.sig,v $
 * Revision 1.2  1998/04/14 14:23:09  george
 *   Removed extra log message
 *
 * Revision 1.1.1.1  1998/04/08 18:39:18  george
 * Version 110.5
 *
 *)
