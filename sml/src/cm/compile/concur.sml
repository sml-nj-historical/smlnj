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

    val fork : (unit -> 'a) -> 'a cond	(* termination condition with value
					 * (thread initially waits with
					 * extremely low urgency) *)
    val wait : 'a cond -> 'a		(* wait with low urgency *)
    val waitU : int -> 'a cond -> 'a	(* wait with given urgency,
					 * (urgency is always higher than
					 * when waiting using "wait") *)

    val inputReady : TextIO.instream -> unit cond
    val ucond : unit -> unit cond
    val signal : unit cond -> unit

    (* forget all waiting threads and input conditions *)
    val reset : unit -> unit
end

structure Concur :> CONCUR = struct

    type tstate = unit SMLofNJ.Cont.cont * int

    datatype 'a cstate =
	Arrived of 'a			(* value *)
      | Waiting of tstate list		(* waiting threads *)

    type 'a cond = 'a cstate ref

    (* simple and brain-dead priority queue *)
    type task_queue = tstate list ref

    fun enqueue (x as (_, xu), qr as ref q) = let
	fun insert [] = [x]
	  | insert ((h as (_, hu)) :: r) =
	    if xu > hu then x :: h :: r else h :: insert r
    in
	qr := insert q
    end

    fun dequeue (ref []) = NONE
      | dequeue (qr as ref (h :: t)) = (qr := t; SOME h)

    val runable : task_queue = ref []
    val inputs = ref ([]: (unit cond * OS.IO.poll_desc) list)

    fun reset () = (runable := []; inputs := [])

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
		  | SOME (ts, _) => SMLofNJ.Cont.throw ts ()
	    end

    fun schedule () =
	case dequeue runable of
	    NONE => schedule_inputs ()
	  | SOME (ts, _) => SMLofNJ.Cont.throw ts ()

    fun wait' _ (ref (Arrived x)) = x
      | wait' u (c as ref (Waiting tsl)) =
	(SMLofNJ.Cont.callcc (fn ts => (c := Waiting ((ts, u) :: tsl);
					schedule ()));
	 wait' u c)

    fun wait c = wait' 0 c

    fun waitU u c = wait' (u + 1) c

    fun fork worker = let
	val c = ref (Waiting [])
    in
	(* We give new workers a low priority so that any threads that
	 * were already running but are now waiting for some event
	 * get control first if they are re-enabled.  This is because
	 * waiting threads will clean up after errors in which case
	 * we don't want new threads to run off. *)
	SMLofNJ.Cont.callcc (fn return =>
	  (SMLofNJ.Cont.callcc (fn ts => (enqueue ((ts, ~1), runable);
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
