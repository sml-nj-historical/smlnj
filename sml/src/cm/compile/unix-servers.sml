(*
 * Handling compile-servers under Unix- (and Unix-like) operating systems.
 *
 *  This is still rather crude and not very robust.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure Servers :> SERVERS = struct

    type pathtrans = (string -> string) option
    type server = (string * Unix.proc) * pathtrans

    val enabled = ref false
    val nservers = ref 0
    val all = ref (StringMap.empty: server StringMap.map)

    val idle = ref ([]: server list)
    val someIdle = ref (Concur.ucond ())

    fun fname (n, (_, NONE)) = n
      | fname (n, (_, SOME f)) = if OS.Path.isAbsolute n then f n else n

    fun servName ((n, _), _) = n

    fun send (s, msg) = let
	val ((name, p), _) = s
	val (_, outs) = Unix.streamsOf p
	fun send0 m =
	    (Say.dsay ["-> ", name, " : ", m];
	     TextIO.output (outs, m))
    in
	send0 msg;
	TextIO.flushOut outs
    end

    fun show_idle () =
	Say.dsay ("Idle:" ::
		  foldr (fn (s, l) => " " :: servName s :: l) ["\n"] (!idle))

    (* Mark a server idle; signal all those who are currently waiting for
     * that...*)
    fun mark_idle s =
	(idle := s :: !idle;
	 Concur.signal (!someIdle);
	 Say.dsay ["Scheduler: slave ", servName s, " has become idle.\n"];
	 show_idle ())

    (* Grab an idle server; wait if necessary; reinitialize condition
     * if taking the only server. *)
    fun grab () =
	case !idle of
	    [] => (Concur.wait (!someIdle); grab ())
	  | [only] =>
		(Say.dsay ["Scheduler: taking last idle slave (",
			   servName only, ").\n"];
		 idle := [];
		 someIdle := Concur.ucond ();
		 only)
	  | first :: more =>
		(Say.dsay ["Scheduler: taking idle slave (",
			   servName first, ").\n"];
		 idle := more;
		 show_idle ();
		 first)

    fun wait_status (s, echo) = let
	val ((name, p), _) = s
	val (ins, _) = Unix.streamsOf p

	fun unexpected l = let
	    fun word (w, l) = " " :: w :: l
	in
	    Say.say ("! Unexpected response from slave " ::
		     name :: ":" :: foldr word ["\n"] l)
	end
	     
	fun crashed () =
	    (Say.say ["! Slave ", name, " has crashed\n"];
	     Unix.reap p)

	val show =
	    if echo then (fn report => Say.say (rev report))
	    else (fn _ => ())

	fun wouldBlock () =
	    case TextIO.canInput (ins, 1) of
		NONE => true
	      | SOME 0 => true
	      | SOME _ => false

	fun loop report =
	    if wouldBlock () then wait report
	    else let
		val line = TextIO.inputLine ins
	    in
		if line = "" then (crashed (); false)
		else
		    (Say.dsay ["<- ", name, ": ", line];
		     case String.tokens Char.isSpace line of
			 ["SLAVE:", "ok"] =>
			     (mark_idle s; show report; true)
		       | ["SLAVE:", "error"] =>
			     (mark_idle s;
			      (* In the case of error we don't show
			       * the report because it will be re-enacted
			       * locally. *)
			      false)
		       | "SLAVE:" :: l => (unexpected l;
					   loop report)
		       | _ => loop (line :: report))
	    end

	and wait report = (Say.dsay ["Scheduler: ", name,
				     " is waiting for slave response.\n"];
			   Concur.wait (Concur.inputReady ins);
			   loop report)
    in
	loop []
    end

    (* Send a "ping" to all servers and wait for the "pong" responses.
     * This should work for all servers, busy or no.  Busy servers will
     * take longer to respond because they first need to finish what
     * they are doing.
     * We use wait_all after we receive an interrupt signal.  The ping-pong
     * protocol does not suffer from the race condition that we would have
     * if we wanted to only wait for "ok"s from currently busy servers.
     * (The race would happen when an interrupt occurs between receiving
     * "ok" and marking the corresponding slave idle). *)
    fun wait_all () = let
	val al = StringMap.listItems (!all)
	fun ping (s as ((name, p), _)) = let
	    val (ins, _) = Unix.streamsOf p
	    fun loop () = let
		val line = TextIO.inputLine ins
	    in
		case String.tokens Char.isSpace line of
		    ["SLAVE:", "pong"] => ()
		  | _ => loop ()
	    end
	in
	    send (s, "ping\n");
	    loop ()
	end
    in
	app ping al;
	idle := al
    end

    fun shutdown (name, method) = let
	val (m, s) = StringMap.remove (!all, name)
	val ((_, p), _) = s
	val (_, il) = List.partition (fn ((n, _), _) => name = n) (!idle)
    in
	method s;
	ignore (Unix.reap p);
	all := m;
	nservers := !nservers - 1;
	idle := il
    end handle LibBase.NotFound => ()

    fun stop name =
	shutdown (name, fn s => send (s, "shutdown\n"))

    fun kill name =
	shutdown (name, fn ((_, p), _) => Unix.kill (p, Posix.Signal.kill))

    fun start { name, cmd, pathtrans } = let
	val _ = stop name
	val p = Unix.execute cmd
	val s : server = ((name, p), pathtrans)
    in
	if wait_status (s, false) then
	    (all := StringMap.insert (!all, name, s);
	     nservers := 1 + !nservers;
	     true)
	else false
    end
	
    fun compile p =
	if not (!enabled) orelse !nservers = 0 then false
	else let
	    val f = SrcPath.osstring p
	    val s = grab ()
	in
	    Say.vsay ["[(", servName s, "): compiling ", f, "]\n"];
	    send (s, concat ["compile ", fname (f, s), "\n"]);
	    wait_status (s, true)
	end

    fun reset () = (Concur.reset (); wait_all ())

    fun startAll st = let
	val _ = reset ()		(* redundant? *)
	val l = !idle
	val _ = idle := []
    in
	app st l
    end

    fun cm p = let
	val d = OS.FileSys.getDir ()
	val f = SrcPath.osstring p
	fun st s =
	    (Say.vsay ["[(", servName s, "): project ", f, "]\n"];
	     send (s, concat ["cm ", fname (d, s), " ", fname (f, s), "\n"]);
	     ignore (wait_status (s, false)))
    in
	startAll st
    end

    fun cmb { archos, dirbase = db } = let
	val d = OS.FileSys.getDir ()
	fun st s =
	    (Say.vsay ["[(", servName s, "): btcompile for ", archos,
		       ", dirbase = ", db, "]\n"];
	     send (s, concat ["cmb ", archos, " ",
			      fname (d, s), " ", db, "\n"]);
	     ignore (wait_status (s, false)))
    in
	startAll st
    end

    fun enable () = enabled := true
    fun disable () = enabled := false

    fun withServers f =
	SafeIO.perform { openIt = enable,
			 closeIt = disable,
			 work = f,
			 cleanup = reset }
end
