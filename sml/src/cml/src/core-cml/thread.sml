(* thread.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure Thread : sig
    include THREAD
    val defaultExnHandler : (exn -> unit) ref
    val reset : bool -> unit
  end = struct

    structure R = RepTypes
    structure S = Scheduler

    datatype thread_id = datatype R.thread_id
    datatype cvar = datatype R.cvar
    datatype cvar_state = datatype R.cvar_state

    type 'a event = 'a R.event

    local
      val tidCount = ref 0
      fun cvar () = CVAR(ref(CVAR_unset []))
    in

    fun reset running = (
	  tidCount := 0;
	  S.reset running)

    fun exnHandler (exn : exn) = ()

    val defaultExnHandler = ref exnHandler

    fun newTId () = let val n = !tidCount
	  in
	    tidCount := n+1;
	    TID{
		id = n,
		alert = ref false,
		done_comm = ref false,
		exnHandler = ref(! defaultExnHandler),
		dead = cvar()
	      }
	  end
    end (* local *)

    fun sameTid (TID{id=a, ...}, TID{id=b, ...}) = (a = b)

    fun compareTid (TID{id=a, ...}, TID{id=b, ...}) = Int.compare(a, b)

    fun hashTid (TID{id, ...}) = Word.fromInt id

    val tidToString = R.tidToString

    fun notifyAndDispatch (TID{dead, ...}) = (
	  S.atomicBegin(); Event.atomicCVarSet dead; S.atomicDispatch())

    fun doHandler (TID{exnHandler, ...}, exn) =
	  ((!exnHandler) exn) handle _ => ()

(** Eventually, this should be:
    fun spawnc f x = let
	  val _ = S.atomicBegin()
	  val id = newTId()
	  fun thread () = (
		(f x) handle ex => doHandler(id, ex);
		notifyAndDispatch id)
	  in
	    SMLofNJ.Cont.callcc (fn parentK => (
	      S.enqueueAndSwitchCurThread(parentK, id);
	      S.atomicEnd();
	      SMLofNJ.Cont.throw (SMLofNJ.Cont.isolate thread) ()));
	    id
	  end
 **)
    fun spawnc f x = let
	  val _ = S.atomicBegin()
	  val id = newTId()
	  in
	    SMLofNJ.Cont.callcc (fn parentK => (
	      S.enqueueAndSwitchCurThread(parentK, id);
	      S.atomicEnd();
	      (f x) handle ex => doHandler(id, ex);
	      notifyAndDispatch id));
	    id
	  end

    fun spawn f = spawnc f ()

    fun joinEvt (TID{dead, ...}) = Event.cvarGetEvt dead

    val getTid = S.getCurThread

    fun exit () = notifyAndDispatch(getTid())

    fun yield () = SMLofNJ.Cont.callcc (fn k => (
	  S.atomicBegin();
	  S.atomicYield k))

  end;
