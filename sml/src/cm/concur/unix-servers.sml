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
    datatype server = S of { name: string,
			     proc: Unix.proc,
			     pt: pathtrans,
			     pref: int }

    val enabled = ref false
    val nservers = ref 0
    val all = ref (StringMap.empty: server StringMap.map)

    val idle = ref ([]: server list)
    val someIdle = ref (Concur.pcond ())

    (* This really shouldn't be here, but putting it into SrcPath would
     * create a dependency cycle.  Some better structuring will fix this. *)
    fun isAbsoluteDescr d =
	(case String.sub (d, 0) of #"/" => true | #"%" => true | _ => false)
	handle _ => false

    fun servName (S { name, ... }) = name
    fun servPref (S { pref, ... }) = pref
    fun servPT (S { pt, ... }) = pt
    fun servProc (S { proc, ... }) = proc
    val servIns = #1 o Unix.streamsOf o servProc
    val servOuts = #2 o Unix.streamsOf o servProc

    fun fname (n, s) =
	case servPT s of
	    NONE => n
	  | SOME f => if isAbsoluteDescr n then f n else n

    fun send (s, msg) = let
	val outs = servOuts s
    in
	Say.dsay ["-> ", servName s, " : ", msg];
	TextIO.output (outs, msg);
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
		 someIdle := Concur.pcond ();
		 only)
	  | first :: more => let
		fun best (b, [], rest) = (b, rest)
		  | best (b, s :: r, rest) = let
			val bp = servPref b
			val sp = servPref s
		    in
			if sp > bp then best (s, r, b :: rest)
			else best (b, r, s :: rest)
		    end
		val (b, rest) = best (first, more, [])
	    in
		Say.dsay ["Scheduler: taking idle slave (",
			  servName b, ").\n"];
		idle := rest;
		show_idle ();
		b
	    end

    fun wait_status (s, echo) = let
	val name = servName s
	val ins = servIns s

	fun unexpected l = let
	    fun word (w, l) = " " :: w :: l
	in
	    Say.say ("! Unexpected response from slave " ::
		     name :: ":" :: foldr word ["\n"] l)
	end
	     
	fun crashed () =
	    (Say.say ["! Slave ", name, " has crashed\n"];
	     Unix.reap (servProc s))

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

	and wait report = (Concur.wait (Concur.inputReady ins);
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
    fun wait_all is_int = let
	val al = StringMap.listItems (!all)
	fun ping s = let
	    val name = servName s
	    val ins = servIns s
	    fun loop () = let
		val line = TextIO.inputLine ins
	    in
		Say.dsay ["<- ", name, ": ", line];
		case String.tokens Char.isSpace line of
		    ["SLAVE:", "pong"] => ()
		  | _ => loop ()
	    end
	in
	    send (s, "ping\n");
	    loop ()
	end
	val si = Concur.pcond ()
    in
	if List.null al then ()
	else (Concur.signal si;
	      if is_int then
		  Say.say
		  ["Waiting for attached servers to become idle...\n"]
	      else ());
	app ping al;
	idle := al;
	someIdle := si
    end

    fun shutdown (name, method) = let
	val (m, s) = StringMap.remove (!all, name)
	val p = servProc s
	val (_, il) = List.partition (fn s => name = servName s) (!idle)
    in
	method s;
	ignore (Unix.reap p);
	all := m;
	nservers := !nservers - 1;
	idle := il
    end handle LibBase.NotFound => ()

    fun stop_by_name name = shutdown (name, fn s => send (s, "shutdown\n"))

    fun stop s = stop_by_name (servName s)

    fun kill s = shutdown (servName s,
			   fn s => Unix.kill (servProc s, Posix.Signal.term))

    fun start { name, cmd, pathtrans, pref } = let
	val _ = stop_by_name name
	val p = Unix.execute cmd
	val s = S { name = name, proc = p, pt = pathtrans, pref = pref }
    in
	if wait_status (s, false) then
	    (all := StringMap.insert (!all, name, s);
	     nservers := 1 + !nservers;
	     SOME s)
	else NONE
    end
	
    fun compile p =
	if not (!enabled) orelse !nservers = 0 then false
	else let
	    val s = grab ()
	    val f = fname (p, s)
	in
	    Say.vsay ["[(", servName s, "): compiling ", f, "]\n"];
	    send (s, concat ["compile ", f, "\n"]);
	    wait_status (s, true)
	end

    fun reset is_int = (Concur.reset (); wait_all is_int)

    fun startAll st = let
	val l = !idle
	val _ = idle := []
	val tl = map (fn s => Concur.fork (fn () => st s)) l
    in
	SafeIO.perform { openIt = fn () => (),
			 closeIt = fn () => (),
			 work = fn () => app Concur.wait tl,
			 cleanup = reset }
    end

    fun cd d = let
	fun st s = let
	    val d' = fname (d, s)
	in
	    send (s, concat ["cd ", d', "\n"]);
	    ignore (wait_status (s, false))
	end
    in
	startAll st
    end

    fun cm { archos, project } = let
	fun st s = let
	    val f = fname (project, s)
	in
	    send (s, concat ["cm ", archos, " ", f, "\n"]);
	    ignore (wait_status (s, false))
	end
    in
	startAll st
    end

    fun cmb { archos, root } = let
	fun st s =
	    (send (s, concat ["cmb ", archos, " ", root, "\n"]);
	     ignore (wait_status (s, false)))
    in
	startAll st
    end

    fun dirbase db = let
	fun st s =
	    (send (s, concat ["dirbase ", db, "\n"]);
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

    val name = servName
end
