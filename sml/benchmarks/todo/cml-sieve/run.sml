(* run.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Code to support top-level interactive use of CML.
 *)

signature RUN_CML =
  sig

    structure CML : CONCUR_ML

  (* log/unlog channels and servers for initialization and termination *)
    exception Unlog
    val logChannel : (string * 'a CML.chan) -> unit
    val unlogChannel : string -> unit
    val logServer : (string * (unit -> unit) * (unit -> unit)) -> unit
    val unlogServer : string -> unit
    val unlogAll : unit -> unit

  (* run the system *)
    val doit : ((unit -> unit) * int option) -> unit
    exception Running

  (* export a CML program *)
    val exportFn : (string * ((string list * string list) -> unit) * int option)
	  -> unit

  (* shutdown a run *)
    val shutdown : unit -> 'a
    exception NotRunning

  end (* RUN_CML *)

functor RunCML (CML : INTERNAL_CML) : RUN_CML =
  struct

    exception Unlog

    local
      datatype item = ITEM of {
	  key : string,
	  init : unit -> unit,
	  shut : unit -> unit
	}
      val chanList = ref ([] : item list)
      val serverList = ref ([] : item list)
      fun unlogItem l name = let
	    fun f [] = raise Unlog
	      | f ((x as ITEM{key, ...})::r) = if (name = key) then r else (x :: (f r))
	    in
	      l := f(!l)
	    end
      fun appInit l () = revapp (fn ITEM{init, ...} => init()) (!l)
    in
    fun unlogAll () = (chanList := []; serverList := [])

    val unlogChannel = unlogItem chanList
    fun logChannel(name, ch) = let
	  fun f () = CML.resetChan ch
	  in
	    (unlogChannel name) handle Unlog => ();
	    chanList := ITEM{key=name, init=f, shut=f} :: (!chanList)
	  end

    val unlogServer = unlogItem serverList
    fun logServer (name, f, g) = (
	  (unlogServer name) handle Unlog => ();
	  serverList := ITEM{key=name, init=f, shut=g} :: (!serverList))
    fun cleanChannels () = (CML.resetChan CML.errCh; appInit chanList ())
    val startServers = appInit serverList
    fun shutdownServers () = let
	  fun shut (ITEM{key, shut, ...}) = CML.sync (CML.choose [
		  CML.threadWait(CML.spawn shut),
		  CML.wrap(CML.timeout(CML.TIME{sec=5, usec=0}),
		    fn () => CML.reportError("shutdown "^key^" timeout"))
		])
	  in
	    app shut (!serverList)
	  end
    end (* local *)

  (* run the system *)
    local
      val setitimer = System.Unsafe.CInterface.setitimer
      val running = ref false
      fun msToTime NONE = NONE
        | msToTime (SOME t) = SOME(
	    if t < 10
	      then CML.TIME{sec=0, usec=10000}
	      else CML.TIME{sec=(t quot 1000), usec=((t rem 1000)*1000)})
    in

    exception Running
    fun doit (initialProc, timeq) = let
	  open System.Signals
	  val _ = if (!running) then raise Running else ();
	  val saveHdlr = inqHandler SIGINT
	  val tq = msToTime timeq
	  in
	    callcc (fn done => (
	      setHandler(SIGINT,
		SOME(fn _ => (CML.reportError "\nInterrupt"; CML.restartTimer(); done)));
	      CML.initQueues ();
	      cleanChannels ();
	      CML.shutdown := throw done;
	      running := true;
	      CML.timerOn tq;
	      startServers();
	      CML.spawn initialProc;
	      CML.exit()));
	  (* here on shutdown or ^C *)
	    setHandler(SIGINT, saveHdlr);
	    shutdownServers ();
	    CML.timerOff();
	    cleanChannels ();
	    running := false
	  end

    fun exportFn (name, f, timeq) = let
	  fun cmd args = doit (fn () => (f args), timeq)
	  in
	    IO.exportFn (name, cmd)
	  end

    exception NotRunning
    fun shutdown () = (if !running then (!CML.shutdown)() else (); raise NotRunning)

  (* hook our termination code into the SML/NJ shutdown facility *)
    val _ = let open System.Unsafe.CleanUp
	  fun clean CleanForQuit = if !running
		then (shutdownServers(); CML.timerOff(); running := false)
		else ()
	    | clean _ = ()
	in
	  addCleaner ("ConcurML", clean)
	end

    end (* local *)

    structure CML : CONCUR_ML = CML

  end (* functor RunCML *)
