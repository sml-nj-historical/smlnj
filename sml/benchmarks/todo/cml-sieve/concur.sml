(* concur.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *)

functor ConcurML () : INTERNAL_CML =
  struct

  (* we must use the fully polymorphic versions of callcc, etc. *)
    open SMLofNJ.Cont

    val versionName = "Concurrent ML, version 0.9.6, October 11, 1991"

  (* some utility functions that should be inlined *)
    fun reverse ([], rl) = rl
      | reverse (x :: rest, rl) = reverse(rest, x :: rl)
    fun op o (f, g) = fn x => f(g x)
    fun map f l= let
	fun map' ([], l) = reverse(l, [])
	  | map' (x::r, l) = map'(r, (f x)::l)
	in
	  map' (l, [])
	end
    fun revmap f l= let
	fun map' ([], l) = l
	  | map' (x::r, l) = map'(r, (f x)::l)
	in
	  map' (reverse(l, []),  [])
	end
    fun a @ [] = a
      | a @ b = let
	  fun append ([], l) = reverse(l, b)
	    | append (x::r, l) = append(r, x::l)
	  in
	    append(a, [])
	  end
    fun op before (x, y) = x

  (* queues *)
    datatype 'a queue_t = Q of {front : 'a list ref, rear : 'a list ref}
  (* create a new queue *)
    fun queueNew () = Q{front = ref [], rear = ref []}
  (* queue insert *)
    fun queueInsc (Q{rear, ...}) = fn x => (rear := x :: !rear)
    fun queueIns ((Q{rear, ...}), x) = (rear := x :: !rear)
  (* remove the head of the queue *)
    exception EmptyQ
    fun queueRem (Q{front = ref [], rear = ref []}) = raise EmptyQ
      | queueRem (Q{front = front as (ref []), rear = rear as (ref l)}) = let
	  val (x::r) = reverse(l, [])
	  in
	    front := r; rear := []; x
	  end
      | queueRem (Q{front = front as (ref(x::r)), ...}) = (front := r; x)

  (* report an internal error to std_err *)
    fun reportError msg = let
	  val s = if (ord(msg) = ord("\n"))
	  	then concat["\nCML: ", substring(msg, 1, (size msg)-1), "\n"]
		else concat["CML: ", msg, "\n"]
	  in
	    System.Signals.maskSignals true;
	    IO.output(IO.std_err, s);
	    System.Signals.maskSignals false
	  end

    exception InternalError
    fun error s = (reportError("\nINTERNAL ERROR: "^s); raise InternalError)

  (* timers *)
    structure T : sig
	val earlier : Time.time * Time.time -> bool
	val add_time : Time.time * Time.time -> time
	val zeroTime : Time.time
        val currentTime : unit -> Time.time
	val timerOff : unit -> unit
	val timerOn : Time.time option -> unit
	val restartTimer : unit -> unit
      end = struct
	val earlier = Time.<
	val add_time = Time.+
	val zeroTime = Time.zeroTime
	fun currentTime () = Time.now
        val saveTime = ref (NONE : time option)
	fun timerOff () = setitimer (0, zeroTime, zeroTime)
	fun timerOn t = (
	      saveTime := t;
	      case t of (SOME tq) => setitimer (0, tq, tq) | _ => ())
	fun restartTimer () = 
	      case !saveTime of (SOME tq) => setitimer (0, tq, tq) | _ => ()
      end
    open T


  (* the termination function *)
    val shutdown = ref (fn () => ())


  (* Per-thread descriptors *)
    datatype thread_id = TID of {  (* thread ids *)
	id	   : int,
	death_cond : unit cond_var
      }
  (* condition variables *)
    and 'a cond_var = COND of 'a cond_state ref
    and 'a cond_state
      = COND_unset of (thread_id * bool ref * 'a cont) list
      | COND_set of 'a

  (* channels *)
    datatype 'a chan = CHAN of {
	inq	: (thread_id * 'a cont) chanq,
	outq	: (thread_id * 'a * unit cont) chanq
      }
      withtype 'a chanq = (bool ref * 'a) queue_t

  (* events *)
    datatype abort_fn = NO_ABORT | ABORT of (unit -> unit)
    datatype 'a base_evt = BASE_EVT of {
	  pollfn : unit -> bool,
	  dofn : abort_fn -> 'a,
	  blockfn : (bool ref * abort_fn * (unit -> unit)) -> 'a,
	  abortfn : abort_fn
	}
    datatype 'a event
      = EVT of ('a base_evt list * bool)    (* the boolean is true if one of the *)
					    (* base events has an abort action *)
      | GUARD of (unit -> 'a event)

    fun mkBaseEvt arg = EVT([BASE_EVT arg], false)

    fun applyAbortFn NO_ABORT = ()
      | applyAbortFn (ABORT a) = a()

  (** Thread id creation **)
    val nextId = ref 0
    fun newId () = let val id = !nextId
	  in
	    nextId := id + 1;
	    TID{id = id, death_cond = COND(ref(COND_unset[]))}
	  end
  (* the current thread is represented using the "var" register *)
    val getCurThread : unit -> thread_id = System.Unsafe.getvar
    val setCurThread : thread_id -> unit = System.Unsafe.setvar
    val getTid = getCurThread

  (* the thread ready queue *)
    val rdyQ : (thread_id * unit cont) queue_t = queueNew()
  (* enqueue a ready thread *)
    val enqueue = queueInsc rdyQ
  (* enqueue the current thread *)
    fun enqueueCurThread resume = (enqueue(getTid(), resume))
  (* add the current thread to the ready queue, and return the next one *)
    fun switchCurThread k = let val curP = (getTid(), k)
	  in
	    case rdyQ
	     of Q{front=ref [], rear=ref []} => curP
	      | Q{front=front as (ref []), rear} => let
		  val (x::r) = reverse(!rear, [curP])
		  in
		    front := r; rear := []; x
		  end
	      | Q{front=front as ref (x::r), rear} => (
		  front := r; rear := (curP :: !rear); x)
	  end


  (** I/O wait queues **)
    fun pollFDs (rdfds, wrfds, blocking) = let
	  val t = if blocking then NONE else (SOME zeroTime)
	  val (rd, wr, _) = System.Unsafe.SysIO.select(rdfds, wrfds, [], t)
	  in
	    (rd, wr)
	  end
    (* The list of I/O wait events *)
      datatype io_operation_t = IO_RD | IO_WR
      type io_item = {
	  fd	   : int,		(* the file descriptor *)
	  io_op	   : io_operation_t,	(* the operation being waited for *)
	  kont	   : unit cont,		(* the synchronization continuation and *)
	  id	   : thread_id,		(* the id of the waiting thread *)
	  err_kont : unit cont,		(* the error continuation of the thread *)
	  dirty	   : bool ref		(* the dirty bit *)
	}
      val ioWaitList = ref ([] : io_item list)

    (* project the list of read fds and list of write fds from the I/O wait list. *)
      fun projIO () = let
	    fun f ([] : io_item list, rd, wr) = (rd, wr)
	      | f ({dirty = ref true, ...}::r, rd, wr) = f(r, rd, wr)
	      | f ({io_op = IO_RD, fd, ...}::r, rd, wr) = f(r, fd::rd, wr)
	      | f ({io_op = IO_WR, fd, ...}::r, rd, wr) = f(r, rd, fd::wr)
	    in
	      f(!ioWaitList, [], [])
	    end

    (* check for available I/O operations *)
      fun checkIO shouldBlock = (case projIO()
	     of ([], []) => ()
	      | (rd, wr) => (case pollFDs(rd, wr, shouldBlock)
		   of ([], []) => ()
		    | (rd, wr) => let
			fun f ([], l) = l
			  | f (({dirty = ref true, ...} : io_item)::r, l) = f (r, l)
			  | f ((x as {io_op, fd, kont, id, dirty, ...})::r, l) = let
			      fun look [] = false
				| look (x::r) = if (x = fd)
				    then (
				      enqueue(id, kont);
				      dirty := true;
				      true)
				    else (look r)
			      in
				if (look(case io_op of IO_RD => rd | IO_WR => wr))
				  then f(r, l)
				  else f(r, x::l)
			      end
			in
			  ioWaitList := f(!ioWaitList, [])
			end
		  (* end case *))
	    (* end case *))
	    handle (System.Unsafe.CInterface.SystemCall _) => let
	      open System.Unsafe.SysIO
	      fun testDesc fd = (ftype(DESC fd); false) handle _ => true
	      fun findBadDescs ([], l) = l
		| findBadDescs ((x as {fd, dirty, err_kont, id, ...} : io_item)::r, l) =
		    if (testDesc fd)
		      then (
			enqueue(id, err_kont);
			dirty := true;
			findBadDescs (r, l))
		      else findBadDescs (r, x::l)
	      in
		ioWaitList := findBadDescs(!ioWaitList, []);
		checkIO shouldBlock
	      end

    (* return true if there is at least one clean I/O wait event on the list *)
      fun waitingForIO () = let
	    fun f (l as (({dirty = ref true, ...}::r)) : io_item list) = (f r)
	      | f l = l
	    in
	      case f(!ioWaitList)
	       of [] => (ioWaitList := []; false)
		| l => (ioWaitList := l; true)
	    end


  (** Timer waiting queues **)
    datatype time_wait_t = TIMEWAIT of {
	wait_time : time,
	id : thread_id,
	kont : unit cont,
	dirty : bool ref
      }
    val timeWaitList = ref ([] : time_wait_t list)

  (* insert a timeout event *)
    fun insTimeWait (tim, id, k, flg) = let
	  val item = TIMEWAIT{wait_time=tim, id=id, kont=k, dirty=flg}
	  fun scan [] = [item]
	    | scan ((t as TIMEWAIT{dirty = ref true, ...})::r) = scan r
	    | scan (l as ((t as TIMEWAIT{wait_time, ...})::r)) =
		if (earlier (tim, wait_time))
		  then (item::l)
		  else (t::(scan r))
	  in
	    timeWaitList := scan(!timeWaitList)
	  end

  (* schedule any threads waiting for times earlier than the current time. *)
    fun remTimeWait () = let
	  val tim = currentTime()
	  fun scan [] = []
	    | scan (l as ((t as TIMEWAIT{dirty = ref true, ...})::r)) = scan r
	    | scan (l as ((t as TIMEWAIT{wait_time, id, kont, dirty})::r)) =
		if earlier(tim, wait_time)
		  then l
		  else (enqueue(id, kont); dirty := true; scan r)
	  in
	    timeWaitList := scan(!timeWaitList)
	  end

  (* return true if there is at least one clean timeout event on the list *)
    fun waitingForTimeout () = let
	  fun f (TIMEWAIT{dirty = ref true, ...}::r) = (f r)
	    | f l = l
	  in
	    case (f (!timeWaitList))
	     of [] => (timeWaitList := []; false)
	      | l => (timeWaitList := l; true)
	  end


  (** Process scheduling (and atomic regions) **)

  (*  test for blocked threads that could conceivably become unblocked *)
    fun checkWaitingThreads () = (
	 case (!ioWaitList) of [] => () | _=> (checkIO false);
	 case (!timeWaitList) of [] => () | _=> remTimeWait ())

  (* atomic regions (just SIGALRM for now) *)
    local
      open System.Signals
      datatype atomic_state = NonAtomic | Atomic | SignalPending
      val atomicState = ref NonAtomic
      fun inAtomicRegion () = (case !atomicState of NonAtomic => false | _ => true)
      fun signalPending () = (case !atomicState of SignalPending => true | _ => false)
    in
  (* begin an atomic region *)
    fun atomicBegin () = (atomicState := Atomic)
  (* Switch control to a thread, while leaving the atomic region *)
    fun atomicSwitchToThread (id, kont) = (
	  setCurThread id;
	  atomicState := NonAtomic;
	  throw kont ())
  (* end an atomic region *)
    fun atomicEnd () = if signalPending()
	  then (
	    checkWaitingThreads();
	    callcc (fn k => (atomicSwitchToThread (switchCurThread k))))
	  else atomicState := NonAtomic
  (* dispatch a thread while exiting an atomic region *)
    fun atomicDispatch () = let
	  val _ = if signalPending() then checkWaitingThreads() else ();
	  (* wait for I/O or delay when there are no ready threads. *)
	  fun waitForSomething () = (case (waitingForIO(), waitingForTimeout())
	       of (false, false) => (!shutdown)()
		| (_, false) => (timerOff(); checkIO true; restartTimer())
		| _ => (System.Signals.pause(); checkWaitingThreads()))
	  fun dequeue () = (case rdyQ
	       of (Q{front = ref [], rear = ref []}) => (waitForSomething(); dequeue())
		| (Q{front = front as (ref []), rear = rear as (ref l)}) => let
		    val (x::r) = reverse(l, [])
		    in
		      front := r; rear := []; x
		    end
		| (Q{front = front as (ref(x::r)), ...}) => (front := r; x))
	  in
	    atomicSwitchToThread (dequeue())
	  end
    fun dispatch () = (atomicBegin(); atomicDispatch())
  (* throw to a continuation while exiting an atomic region *)
    fun atomicThrow (k, x)= if signalPending()
	  then (
	    checkWaitingThreads();
	    callcc (fn k1 => (atomicSwitchToThread(switchCurThread k1)));
	    throw k x)
	  else (
	    atomicState := NonAtomic;
	    throw k x)
  (* initialize the atomic region support *)
    fun initAtomic () = let
	  val checkIOKont = callcc (fn k1 => (
		callcc (fn k2 => (throw k1 k2));
		(* NOTE: we start in an atomic region *)
		checkWaitingThreads(); atomicDispatch()))
	  fun alrm_handler (_, k) = if inAtomicRegion()
		then (atomicState := SignalPending;  k)
		else (enqueueCurThread k; atomicBegin(); checkIOKont)
	  in
	    setHandler (SIGALRM, SOME alrm_handler);
	    atomicState := NonAtomic
	  end
    end (* local *)

  (* return the # of threads created and the length of the ready queue. *)
    fun load () = let
	  val _ = atomicBegin()
	  val Q{front, rear} = rdyQ
	  val res = (!nextId, List.length(!front) + List.length(!rear))
	  in
	    atomicEnd();  res
	  end


  (** Condition variables **)
    exception WriteTwice
    fun condVar () = COND(ref(COND_unset[]))
    fun writeVar (COND rc, x) = (
	  atomicBegin();
	  case (! rc)
	   of (COND_unset pl) => let
		fun f [] = ()
		  | f ((_, ref true, _) :: r) = (f r)
		  | f ((id, flg, kont) :: r) = (
		      enqueue (id, callcc (fn k1 => (
			callcc (fn k2 => throw k1 k2);
			throw kont x)));
		      flg := true;
		      f r)
		in
		  rc := (COND_set x);
		  f pl;
		  atomicEnd ()
		end
	    | _ => (atomicEnd(); raise WriteTwice)
	  (* end case *))
    fun addCondWaiter ([], w) = [w]
      | addCondWaiter ((_, ref true, _)::r, w) = addCondWaiter (r, w)
      | addCondWaiter (x::r, w) = x::(addCondWaiter(r, w))
    fun readVar (COND rc) = (
	  atomicBegin();
	  case !rc
	   of (COND_set x) => (atomicEnd(); x)
	    | (COND_unset pl) => callcc (fn k => (
		rc := COND_unset(addCondWaiter(pl, (getTid(), ref false, k)));
		atomicDispatch()))
	  (* end case *))
    fun readVarEvt (COND rc) = mkBaseEvt {
	    pollfn = fn () => (case !rc of (COND_set _) => true | _ => false),
	    dofn = fn abortfn => (
	      case !rc
	       of (COND_set x) => (atomicEnd(); applyAbortFn abortfn; x)
		| _ => error "[readVarEvt.dofn]"),
	    blockfn = fn (dirty, abortfn, next) => let
	      fun block k = (case !rc
		   of (COND_unset pl) => (
			rc := COND_unset(addCondWaiter(pl, (getTid(), dirty, k)));
			next(); error "[readVarEvt]")
		    | _ => error "[readVarEvt.blockfn]")
	      in
		case abortfn
		 of NO_ABORT => (callcc block)
		  | (ABORT f) => ((callcc block) before (f ()))
	      end,
	    abortfn = NO_ABORT
	  }


  (** Channel operations **)
    fun insert (q : 'a chanq, flg, item) = queueIns(q, (flg, item))
    fun remove (q : 'a chanq) = let
	  val (flg, item) = queueRem q
	  in
	    flg := true; item
	  end

  (* Clean a channel of satisfied transactions.	 We do this incrementally to
   * give an amortized constant cost.  Basically we guarantee that the front
   * of the queue will be unsatisfied.	Return true if the resulting queue
   * is non-empty.
   *)
    local
      fun clean' [] = []
	| clean' ((ref true, _) :: r) = clean' r
	| clean' l = l
    in
    fun clean ((Q{front, rear}) : 'a chanq) = (case (front, rear)
	 of (ref [], ref []) => false
	  | (ref [], ref r) => (case clean'(reverse(r, []))
	     of [] => (rear := []; false)
	      | l => (front := l; rear := []; true))
	  | (ref f, ref r) => (case (clean' f)
	     of [] => (case clean'(reverse(r, []))
		 of [] => (front := []; rear := []; false)
		  | l => (front := l; rear := []; true))
	      | l => (front := l; true)))
    fun cleanAndRemove ((Q{front, rear}) : 'a chanq) = (case (front, rear)
	 of (ref [], ref []) => NONE
	  | (ref [], ref r) => (case clean'(reverse(r, []))
	     of [] => (rear := []; NONE)
	      | ((flg, item)::rest) => (
		  front := rest; rear := []; flg := true; SOME item))
	  | (ref f, ref r) => (case (clean' f)
	     of [] => (case clean'(reverse(r, []))
		 of [] => (front := []; rear := []; NONE)
		  | ((flg, item)::rest) => (
		      front := rest; rear := []; flg := true; SOME item))
	      | ((flg, item)::rest) => (front := rest; flg := true; SOME item)))
    end (* local *)

  (* remove any waiting threads from a channel's queues *)
    fun resetChan (CHAN{inq=Q{front=f1, rear=r1}, outq=Q{front=f2, rear=r2}}) = (
	  f1 := []; r1 := []; f2 := []; r2 := [])

  (* channel : unit -> '1a chan *)
    fun channel () = CHAN{inq = queueNew(), outq = queueNew()}

  (* sameChannel : ('a chan * 'a chan) -> bool *)
    fun sameChannel (CHAN{inq=Q{front=f1, ...}, ...}, CHAN{inq=Q{front=f2, ...}, ...}) =
	  (f1 = f2)

  (* send : ('a chan * 'a) -> unit *)
    fun send (CHAN{inq, outq}, msg) = callcc (fn send_k => (
	  atomicBegin();
	  case (cleanAndRemove inq)
	   of SOME(rid, rkont) => (
		enqueueCurThread send_k;
		setCurThread rid;
		atomicThrow (rkont, msg))
	    | NONE => (
		insert(outq, ref false, (getTid(), msg, send_k));
		atomicDispatch())
	  (* end case *)))
    fun sendc ch msg = send (ch, msg)

  (* accept : 'a chan -> 'a *)
    fun accept (CHAN{inq, outq}) = callcc (fn accept_k => (
	  atomicBegin();
	  case (cleanAndRemove outq)
	   of SOME(sid, msg, skont) => (
		enqueue (sid, skont);
		atomicThrow (accept_k, msg))
	    | NONE => (
		insert(inq, ref false, (getTid(), accept_k));
		atomicDispatch())
	  (* end case *)))

  (* transmit : ('a chan * 'a) -> unit event *)
    fun transmit (CHAN{inq, outq}, msg) = let
	  fun pollFn () = (clean inq)
	  fun doFn abortfn = let
		val (rid, rkont) = remove inq
		fun doit k = (
		      enqueueCurThread k;
		      setCurThread rid;
		      atomicThrow(rkont, msg))
		in
		  case abortfn
		   of NO_ABORT => callcc doit
		    | (ABORT f) => (callcc doit; f())
		end
	  fun blockFn (flg, abortfn, next) = let
		fun block k = (
		      clean outq;
		      insert(outq, flg, (getTid(), msg, k));
		      next(); error "[transmit]")
		in
		  case abortfn
		   of NO_ABORT => (callcc block)
		    | (ABORT f) => (callcc block; f())
		end
	  in
	    mkBaseEvt {
		pollfn = pollFn, dofn = doFn, blockfn = blockFn, abortfn = NO_ABORT
	      }
	  end
    fun transmitc ch msg = transmit (ch, msg)

  (* receive : 'a chan -> 'a event *)
    fun receive (CHAN{inq, outq}) = let
	  fun pollFn () = (clean outq)
	  fun doFn abortfn = let
		val (sid, msg, skont) = remove outq
		in
		  enqueue (sid, skont);
		  atomicEnd ();
		  applyAbortFn abortfn;
		  msg
		end
	  fun blockFn (flg, abortfn, next) = let
		fun block k = (
		      clean inq;
		      insert(inq, flg, (getTid(), k));
		      next(); error "[receive]")
		in
		  case abortfn
		   of NO_ABORT => (callcc block)
		    | (ABORT f) => ((callcc block) before (f ()))
		end
	  in
	    mkBaseEvt {
		pollfn = pollFn, dofn = doFn, blockfn = blockFn, abortfn = NO_ABORT
	      }
	  end

  (* A channel to pass errors on to an error monitor *)
    val errCh : (thread_id * exn) chan = channel()


  (** Thread operations **)
    fun notify () = let
	  val (TID{id, death_cond, ...}) = getTid()
	  in
	    writeVar (death_cond, ())
	  end
    fun spawnc f x = let
	  val _ = atomicBegin()
	  val id = newId()
	  in
	    callcc (fn parent_k => (
	      enqueueCurThread parent_k;
	      setCurThread id;
	      atomicEnd();
	      (f x; notify())
		handle ex => (notify (); send (errCh, ((getTid()), ex)));
	      dispatch ()));
	    id
	  end
    fun spawn f = spawnc f ()

    fun fastSpawn f = let
	  val _ = atomicBegin()
	  val id = newId()
	  in
	    callcc (fn k => (
	      callcc (fn k' => (enqueue (id, k'); atomicThrow (k, ())));
	      (f (); notify())
		handle ex => (notify (); send (errCh, (getTid(), ex)));
	      dispatch ()))
	  end
    val spawnList = app fastSpawn

  (* terminate the current thread *)
    fun exit () = (notify(); dispatch())

    fun threadWait (TID{death_cond, ...}) = readVarEvt death_cond

  (* test thread_ids for equality *)
    fun sameThread (TID{id=p1, ...}, TID{id=p2, ...}) = (p1 = p2)
  (* test the order of thread ids *)
    fun tidLessThan (TID{id=p1, ...}, TID{id=p2, ...}) = (p1 < p2)
  (* return a string representation of a thread id *)
    fun tidToString (TID{id, ...}) = concat ["[", makestring id, "]"]

  (* yield control to the next thread *)
    fun yield () = callcc (fn k => (
	  atomicBegin(); enqueueCurThread k; atomicDispatch()))


  (** Event operations **)
    fun dummyFn _ = error "[dummyFn]"

  (* always : 'a -> 'a event *)
    fun always x = mkBaseEvt {
	    pollfn = (fn () => true),
	    dofn = (fn abort => (applyAbortFn abort; x)),
	    blockfn = dummyFn,
	    abortfn = NO_ABORT
	  }
    val ALWAYS = always ()

  (* 'a event list -> 'a event *)
    fun choose l = let
	  fun f ([], el, [], flg) = EVT(el, flg)
	  (* note that the guard list gl is in reverse order *)
	    | f ([], el, gl, flg) =
		GUARD(fn () =>
		  choose (EVT(el, flg) :: (revmap (fn g => (g ())) gl)))
	    | f (EVT(el', false) :: r, el, gl, hasAbort) =
		f (r, el' @ el, gl, hasAbort)
	    | f (EVT(el', true) :: r, el, gl, _) = f (r, el' @ el, gl, true)
	    | f ((GUARD g)::r, el, gl, hasAbort) = f (r, el, g::gl, hasAbort)
	  in
	    f (l, [], [], false)
	  end

  (* guard : (unit -> 'a event) -> 'a event *)
    val guard = GUARD

  (* wrap : ('a event * ('a -> 'b)) -> 'b event *)
    fun wrap (GUARD g, f) = GUARD(fn () => wrap(g (), f))
      | wrap (EVT(el, flg), f) = let
	  fun wrapEvts ([], l) = l
	    | wrapEvts ((BASE_EVT{pollfn, dofn, blockfn, abortfn})::r, l) = let
		val bev = BASE_EVT{
			pollfn = pollfn,
			dofn = (f o dofn),
			blockfn = (f o blockfn),
			abortfn = abortfn
		      }
		in
		  wrapEvts(r, bev::l)
		end
	  in
	    EVT(wrapEvts (el, []), flg)
	  end

  (* wrapHandler : ('a event * (exn -> 'a)) -> 'a event *)
    fun wrapHandler (GUARD g, h) =GUARD(fn () => wrapHandler(g (), h))
      | wrapHandler (EVT(el, flg), h) = let
	  fun wh f x = (f x) handle e => (h e)
	  fun wrapEvts ([], l) = l
	    | wrapEvts ((BASE_EVT{pollfn, dofn, blockfn, abortfn})::r, l) = let
		val bev = BASE_EVT{
			pollfn = pollfn,
			dofn = (wh dofn),
			blockfn = (wh blockfn),
			abortfn = abortfn
		      }
		in
		  wrapEvts(r, bev::l)
		end
	  in
	    EVT(wrapEvts (el, []), flg)
	  end

  (* wrapAbort : (a event * (unit -> unit)) -> 'a event *)
    fun wrapAbort (GUARD g, abort) = GUARD(fn () => wrapAbort (g (), abort))
      | wrapAbort (EVT(el, flg), abort) = let
	  fun addAbortFn (BASE_EVT{pollfn, dofn, blockfn, abortfn}, abort) = BASE_EVT{
		  pollfn = pollfn,
		  dofn = dofn,
		  blockfn = blockfn,
		  abortfn = (case abortfn
		     of NO_ABORT => ABORT abort
		      | (ABORT a) => ABORT(fn () => (fastSpawn abort; a())))
		}
	  in
	    case el
	     of [] => EVT([], false)
	      | [bev] => EVT([addAbortFn (bev, abort)], true)
	      | (leader :: followers) => let
		  val n = length followers
		  in
		    GUARD (fn () => let
		      val ackCh = channel()
		      fun followerAbort () = send(ackCh, ())
		      fun leaderAbort 0 = abort()
		        | leaderAbort i = (accept ackCh; leaderAbort(i-1))
		      in
			EVT(
			  addAbortFn(leader, fn () => (leaderAbort n)) ::
			    (map (fn b => addAbortFn(b, followerAbort)) followers),
			  true)
		      end)
		  end
	  end

  (** Sync and poll **)
    local
    (* Generate index numbers for "non-deterministic" selection.  We use a
     * round-robin style policy. *)
      val cnt = ref 0
      fun random 1 = 0
	| random i = let val j = !cnt
	    in
	      cnt := Bits.andb(j+1, 0x7fff); (j rem i)
	    end
      fun selectDoFn (el, n) = let
	    fun sel (f::_, 0) = f NO_ABORT
	      | sel (_::r, n) = sel (r, n-1)
	    in
	      sel (el, random n)
	    end

    fun syncOnEvts ([], _) = exit()
      | syncOnEvts ([BASE_EVT{pollfn, dofn, blockfn, ...}], _) = (
	    atomicBegin();
	    if (pollfn ())
	      then dofn NO_ABORT
	      else blockfn (ref false, NO_ABORT, atomicDispatch))
      | syncOnEvts (bevs, false) = let
	  fun ext ([], blockFns) = capture (fn k => let
		val escape = escape k
		val dirtyFlg = ref false
		fun log [] = atomicDispatch ()
		  | log (blockfn :: r) = escape (
		      blockfn (dirtyFlg, NO_ABORT, fn () => log r))
		in
		  log blockFns; error "[log]"
		end)
	    | ext (BASE_EVT{pollfn, dofn, blockfn, ...} :: r, blockFns) =
		if (pollfn ())
		  then extRdy (r, [dofn], 1)
		  else ext (r, blockfn::blockFns)
	  and extRdy ([], doFns, n) = selectDoFn (doFns, n)
	    | extRdy (BASE_EVT{pollfn, dofn, ...} :: r, doFns, n) =
		if (pollfn ())
		  then extRdy (r, dofn::doFns, n+1)
		  else extRdy (r, doFns, n)
	  in
	    atomicBegin();
	    ext (bevs, [])
	  end
      | syncOnEvts (bevs, true) = let
	  datatype 'a bevt_status
	    = BLK of 'a block_fn_t
	    | BLK_ABORT of ('a block_fn_t * (unit -> unit))
	    | RDY of (abort_fn -> 'a)
	    | RDY_ABORT of ((abort_fn -> 'a) * (unit -> unit))
	  withtype 'a block_fn_t = (bool ref * abort_fn * (unit -> unit)) -> 'a
	  fun ext ([], sts) = let
		fun projAbortFns [] = []
		  | projAbortFns (BLK_ABORT(_, abort)::r) = abort :: (projAbortFns r)
		  | projAbortFns (_::r) = projAbortFns r
		val abortFns = projAbortFns sts
		val allAborts = ABORT(fn () => spawnList abortFns)
		fun mkAbortFn i = let
		      fun abort ([], _) = ()
			| abort (a::r, j) = (
			    if (i <> j) then (fastSpawn a; ()) else ();
			    abort (r, j+1))
		      in
			ABORT(fn () => abort(abortFns, 0))
		      end
		in
		  capture (fn k => let
		    val escape = escape k
		    val dirtyFlg = ref false
		    fun log ([], _) = atomicDispatch ()
		      | log ((BLK bfn) :: r, i) = escape (
			  bfn (dirtyFlg, allAborts,
			    fn () => (log(r, i); error "[log]")))
		      | log ((BLK_ABORT(bfn, _)) :: r, i) = escape (
			  bfn (dirtyFlg, mkAbortFn i,
			    fn () => (log(r, i+1); error "[log]")))
		      | log _ = error "[log]"
		    in
		      log (sts, 0)
		    end)
		end
	    | ext (BASE_EVT{pollfn, dofn, blockfn, abortfn} :: r, sts) = (
		case (pollfn(), abortfn)
		 of (false, NO_ABORT) => ext (r, (BLK blockfn)::sts)
		  | (false, ABORT a) => ext (r, (BLK_ABORT(blockfn, a))::sts)
		  | (true, NO_ABORT) => extRdy (r, (RDY dofn)::sts, 1)
		  | (true, ABORT a) => extRdy (r, (RDY_ABORT(dofn, a))::sts, 1)
		(* end case *))
	  and extRdy ([], sts, nRdy) = let
		fun selAndAbortRest ([], _, _) = error "[selAndAbortRest]"
		  | selAndAbortRest ((BLK _)::r, i, abortFns) =
		      selAndAbortRest (r, i, abortFns)
		  | selAndAbortRest ((BLK_ABORT(_, abort))::r, i, abortFns) =
		      selAndAbortRest (r, i, abort::abortFns)
		  | selAndAbortRest ((RDY doFn)::r, 0, abortFns) =
		      abortRest (r, abortFns, doFn)
		  | selAndAbortRest ((RDY _)::r, i, abortFns) =
		      selAndAbortRest (r, i-1, abortFns)
		  | selAndAbortRest ((RDY_ABORT(doFn, _))::r, 0, abortFns) =
		      abortRest (r, abortFns, doFn)
		  | selAndAbortRest ((RDY_ABORT(_, abort))::r, i, abortFns) =
		      selAndAbortRest (r, i-1, abort::abortFns)
		and abortRest ([], abortFns, doFn) =
		      doFn (ABORT(fn () => spawnList abortFns))
		  | abortRest ((BLK_ABORT(_, abort))::r, abortFns, doFn) =
		      abortRest (r, abort::abortFns, doFn)
		  | abortRest ((RDY_ABORT(_, abort))::r, abortFns, doFn) =
		      abortRest (r, abort::abortFns, doFn)
		  | abortRest (_::r, abortFns, doFn) =
		      abortRest (r, abortFns, doFn)
		in
		  selAndAbortRest (sts, random nRdy, [])
		end
	    | extRdy (BASE_EVT{pollfn, dofn, blockfn, abortfn} :: r, sts, nRdy) = (
		case (pollfn(), abortfn)
		 of (false, NO_ABORT) => extRdy (r, sts, nRdy)
		  | (false, ABORT a) => extRdy (r, (BLK_ABORT(blockfn, a))::sts, nRdy)
		  | (true, NO_ABORT) => extRdy (r, (RDY dofn)::sts, nRdy+1)
		  | (true, ABORT a) => extRdy (r, (RDY_ABORT(dofn, a))::sts, nRdy+1)
		(* end case *))
	  in
	    atomicBegin();
	    ext (bevs, [])
	  end

    fun pollEvts ([], _) = NONE
      | pollEvts ([BASE_EVT{pollfn, dofn, ...}], _) = (
	    atomicBegin();
	    if (pollfn ()) then SOME(dofn NO_ABORT) else (atomicEnd(); NONE))
      | pollEvts (bevs, false) = let
	  fun ext [] = (atomicEnd(); NONE)
	    | ext (BASE_EVT{pollfn, dofn, ...} :: r) =
		if (pollfn ()) then extRdy(r, [dofn], 1) else ext r
	  and extRdy ([], doFns, n) = SOME(selectDoFn (doFns, n))
	    | extRdy (BASE_EVT{pollfn, dofn, ...} :: r, doFns, n) =
		if (pollfn ())
		  then extRdy (r, dofn::doFns, n+1)
		  else extRdy (r, doFns, n)
	  in
	    atomicBegin();
	    ext bevs
	  end
      | pollEvts (bevs, true) = let
	  datatype 'a bevt_status
	    = BLK_ABORT of (unit -> unit)
	    | RDY of (abort_fn -> 'a)
	    | RDY_ABORT of ((abort_fn -> 'a) * (unit -> unit))
	  fun doAndAbort (sts, i) = let
		fun f ([], abortFns, _, NONE) = (spawnList abortFns; NONE)
		  | f ([], abortFns, _, SOME doFn) =
			SOME (doFn (ABORT(fn () => spawnList abortFns)))
		  | f ((BLK_ABORT abort) :: r, abortFns, i, doFn) =
			f (r, abort::abortFns, i, doFn)
		  | f ((RDY doFn) :: r, abortFns, 0, _) =
			f (r, abortFns, i, SOME doFn)
		  | f ((RDY _) :: r, abortFns, i, doFn) =
			f (r, abortFns, i-1, doFn)
		  | f ((RDY_ABORT(doFn, _)) :: r, abortFns, 0, _) =
			f (r, abortFns, i, SOME doFn)
		  | f ((RDY_ABORT(_, abort)) :: r, abortFns, i, doFn) =
			f (r, abort::abortFns, i, doFn)
		in
		  f (sts, [], i, NONE)
		end
	  fun ext ([], sts) = doAndAbort (sts, ~1)
	    | ext (BASE_EVT{pollfn, dofn, abortfn, ...} :: r, sts) = (
		case (pollfn (), abortfn)
		 of (false, NO_ABORT) => ext (r, sts)
		  | (false, ABORT a) => ext (r, (BLK_ABORT a)::sts)
		  | (true, NO_ABORT) => extRdy (r, (RDY dofn)::sts, 1)
		  | (true, ABORT a) => extRdy (r, (RDY_ABORT(dofn, a))::sts, 1)
		(* end case *))
	  and extRdy ([], sts, n) = doAndAbort (sts, random n)
	    | extRdy (BASE_EVT{pollfn, dofn, abortfn, ...} :: r, sts, n) = (
		case (pollfn (), abortfn)
		 of (false, NO_ABORT) => extRdy (r, sts, n)
		  | (false, ABORT a) => extRdy (r, (BLK_ABORT a)::sts, n)
		  | (true, NO_ABORT) => extRdy (r, (RDY dofn)::sts, n+1)
		  | (true, ABORT a) => extRdy (r, (RDY_ABORT(dofn, a))::sts, n+1)
		(* end case *))
	  in
	    atomicBegin();
	    ext (bevs, [])
	  end

    fun evalGuard (GUARD g) = evalGuard (g ())
      | evalGuard (EVT arg) = arg
    in
    fun sync evt = syncOnEvts (evalGuard evt)
    val select = sync o choose
    fun poll evt = pollEvts (evalGuard evt)
    end (* local *)

  (* waitUntil : time -> unit event *)
    fun waitUntil (t as TIME{sec, usec}) = let
	  val curTime = currentTime()
	  in
	    if earlier(curTime, t)
	      then let
	        fun pollFn () = false
		fun blockFn (flg, abort, next) = let
		      fun block k = (
			    insTimeWait(t, getTid(), k, flg);
			    next())
		      in
			case abort
			 of NO_ABORT => (callcc block)
			  | (ABORT f) => (callcc block; f ())
		      end
		in
		  mkBaseEvt {
		      pollfn = pollFn, dofn = dummyFn, blockfn = blockFn,
		      abortfn = NO_ABORT
		    }
		end
	      else ALWAYS
	  end

  (* timeout : time -> unit event *)
    fun timeout (t as TIME{sec, usec}) = if earlier(t, zeroTime)
	  then ALWAYS
	  else let
	    fun pollFn () = false
	    fun blockFn (flg, abort, next) = let
		  fun block k = (
			insTimeWait(add_time (currentTime(), t), getTid(), k, flg);
			next())
		  in
		    case abort
		     of NO_ABORT => (callcc block)
		      | (ABORT f) => (callcc block; f ())
		  end
	    in
	      mkBaseEvt {
		  pollfn = pollFn, dofn = dummyFn, blockfn = blockFn,
		  abortfn = NO_ABORT
		}
	    end


  (** Initialization **)

  (* initialize the internal queues, I/O waiting lists and counters *)
    fun initQueues () = let val Q{front, rear} = rdyQ
	  in
	    front := []; rear := [];
	    ioWaitList := [];
	    timeWaitList := [];
	    nextId := 0;
	    setCurThread (newId());
	    initAtomic()
	  end

  end (* functor ConcurML *)
