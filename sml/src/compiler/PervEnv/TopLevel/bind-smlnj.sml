(* bind-smlnj.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file binds the SML/NJ specific signatures and structures 
 * in the pervasive top-level environment.
 *
 *)

signature SIGNALS = SIGNALS
structure Signals = Signals

signature CLEAN_UP = CLEAN_UP
signature CONT = CONT
signature INTERVAL_TIMER = INTERVAL_TIMER
signature INTERNALS = INTERNALS
signature GC = GC
signature SYS_INFO = SYS_INFO
signature WEAK = WEAK
signature SML_OF_NJ = SML_OF_NJ

structure SMLofNJ : SML_OF_NJ =
  struct
    open SMLofNJ
    val exportML = Export.exportML
    val exportFn = Export.exportFn
    structure Cont = Cont
    structure IntervalTimer = IntervalTimer
    structure Internals = Internals
    structure SysInfo = SysInfo
    structure Weak = Weak
  end;

(* Lazy can't be a substructure of SMLofNJ because the magical
   property of the susp datatype would be lost in signature
   matching? DBM *)
structure Lazy =
  struct
    datatype susp = datatype PrimTypes.susp
  end

signature UNSAFE_OBJECT = UNSAFE_OBJECT
signature POLL = POLL
signature UNSAFE_ARRAY = UNSAFE_ARRAY
signature UNSAFE_VECTOR = UNSAFE_VECTOR
signature UNSAFE_MONO_ARRAY = UNSAFE_MONO_ARRAY
signature UNSAFE_MONO_VECTOR = UNSAFE_MONO_VECTOR
signature UNSAFE = UNSAFE
structure Unsafe = Unsafe


(*
 * $Log: bind-smlnj.sml,v $
 * Revision 1.3  1998/05/23 14:09:58  george
 *   Fixed RCS keyword syntax
 *
 *
 *)
