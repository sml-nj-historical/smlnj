(*
 * A very simple concurrency package (inspired by CML and the concept of
 * "futures", but much less powerful).
 *   - uses no preemption
 *   - thread gives up control by waiting on a condition
 *   - conditions can signal thread termination, available input on some
 *     text stream, or on some explicitly signalled "unit" condition
 *   - gives total priority to "local" computations
 *     (meaning that all threads must get stuck before I/O is even checked)
 * (This is just here to utilize some external concurrency, i.e., OS
 *  processes.  The processes must synchronize themselves with the
 *  controlling process via I/O.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CONCUR = sig

    type 'a cond			(* condition with value *)

    val fork : (unit -> 'a) -> 'a cond	(* termination condition with value *)
    val wait : 'a cond -> 'a
    val inputReady : TextIO.instream -> unit cond
    val ucond : unit -> unit cond
    val signal : unit cond -> unit

    (* forget all waiting threads and input conditions *)
    val reset : unit -> unit
end

structure Concur :> CONCUR = struct

    type tstate = unit SMLofNJ.Cont.cont

    datatype 'a cstate =
	Arrived of 'a			(* value *)
      | Waiting of tstate list		(* waiting threads *)

    type 'a cond = 'a cstate ref

    type 'a queue = ('a list * 'a list) ref

    fun enqueue (x, q as ref (front, back)) = q := (front, x :: back)

    fun dequeue (ref ([], [])) = NONE
      | dequeue (q as ref (x :: front, back)) = (q := (front, back); SOME x)
      | dequeue (q as ref ([], back)) = (q := (rev back, []); dequeue q)

    val runable : tstate queue = ref ([], [])
    val inputs = ref ([]: (unit cond * OS.IO.poll_desc) list)

    fun reset () = (runable := ([], []); inputs := [])

    (* we heavily favor non-I/O conditions, but that's ok for our purposes *)

    fun wakeup (ref (Arrived _), _) =
	(Say.say ["woken up twice!\n"]; raise Fail "concur")
      | wakeup (r as ref (Waiting tsl), v) =
	(r := Arrived v; app (fn ts => enqueue (ts, runable)) tsl)

    fun ucond () = (ref (Waiting [])) : unit cond
    fun signal (ref (Arrived ())) = ()
      | signal uc = wakeup (uc, ())

    fun schedule_inputs () =
	case !inputs of
	    [] => (Say.say ["deadlock!\n"]; raise Fail "concur")
	  | il => let
		val dl = map #2 il
		(* since nothing else is there to do we can afford to wait *)
		val pil = OS.IO.poll (dl, NONE)
		fun isReady (_, pd) = let
		    val pd_iod = OS.IO.pollToIODesc pd
		    fun sameIod pi =
			OS.IO.compare (pd_iod,
				       OS.IO.pollToIODesc
				         (OS.IO.infoToPollDesc pi)) = EQUAL
		in
		    List.exists sameIod pil
		end
		val (ready, notready) = List.partition isReady il
	    in
		inputs := notready;
		app (fn (c, _) => wakeup (c, ())) ready;
		(* try to schedule again; if this fails it's bad *)
		case dequeue runable of
		    NONE =>
			(Say.say
			 ["schedule_inputs failed to wake anybody up!\n"];
			 raise Fail "concur")
		  | SOME ts => SMLofNJ.Cont.throw ts ()
	    end

    fun schedule () =
	case dequeue runable of
	    NONE => schedule_inputs ()
	  | SOME ts => SMLofNJ.Cont.throw ts ()

    fun wait (ref (Arrived x)) = x
      | wait (c as ref (Waiting tsl)) =
	(SMLofNJ.Cont.callcc (fn ts => (c := Waiting (ts :: tsl);
					schedule ()));
	 wait c)

    fun fork worker = let
	val c = ref (Waiting [])
    in
	SMLofNJ.Cont.callcc (fn return =>
	  (SMLofNJ.Cont.callcc (fn ts => (enqueue (ts, runable);
					  SMLofNJ.Cont.throw return c));
	   wakeup (c, worker ());
	   schedule ()))
    end

    fun inputReady iis = let
	val fis = TextIO.getInstream iis
	val (r, v) = TextIO.StreamIO.getReader fis
	fun bad () = (Say.say ["inputReady: bad stream\n"];
		      raise Fail "concur")
    in
	case r of
	    TextPrimIO.RD { ioDesc = SOME d, ... } =>
		(case OS.IO.pollDesc d of
		     NONE => bad ()
		   | SOME pd => let
			 val pd = OS.IO.pollIn pd
			 val fis' = TextIO.StreamIO.mkInstream (r, v)
			 val c = ref (Waiting [])
		     in
			 inputs := (c, pd) :: !inputs;
			 TextIO.setInstream (iis, fis');
			 c
		     end)
	  | _ => bad ()
    end
end
