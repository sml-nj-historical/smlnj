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
    type server = (string * Unix.proc * string list ref) * pathtrans

    val enabled = ref false
    val nservers = ref 0
    val all = ref (StringMap.empty: server StringMap.map)

    val idle = ref ([]: server list)
    val someIdle = ref (Concur.ucond ())

    fun fname (n, (_, NONE)) = n
      | fname (n, (_, SOME f)) = if OS.Path.isAbsolute n then f n else n

    fun servName ((n, _, _), _) = n

    fun send (s, msg) = let
	val ((name, p, r as ref el), _) = s
	val (_, outs) = Unix.streamsOf p
	fun send0 m =
	    (Say.dsay ["-> ", name, " : ", m];
	     TextIO.output (outs, m))
	fun ev x = send0 (concat ["evict ", x, "\n"])
    in
	app ev el;
	r := [];
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
	val ((name, p, _), _) = s
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

	fun loop report =
	    case TextIO.canInput (ins, 1) of
		NONE => wait report
	      | SOME 0 => wait report
	      | SOME _ => let
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

    fun stop name = let
	val (m, s) = StringMap.remove (!all, name)
	val ((_, p, _), _) = s
    in
	send (s, "shutdown\n");
	ignore (Unix.reap p);
	all := m;
	nservers := !nservers - 1
    end handle LibBase.NotFound => ()

    fun kill name = let
	val (m, s) = StringMap.remove (!all, name)
	val ((_, p, _), _) = s
    in
	Unix.kill (p, Posix.Signal.kill);
	ignore (Unix.reap p);
	all := m;
	nservers := !nservers - 1
    end handle LibBase.NotFound => ()

    fun start { name, cmd, pathtrans } = let
	val _ = stop name
	val p = Unix.execute cmd
	val s : server = ((name, p, ref []), pathtrans)
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
	    Say.vsay ["(", servName s, "): compiling ", f, "\n"];
	    send (s, concat ["compile ", fname (f, s), "\n"]);
	    wait_status (s, true)
	end

    fun reset () = let
	fun busy s =
	    not (List.exists (fn s' => servName s = servName s') (!idle))
	val b = List.filter busy (StringMap.listItems (!all))
	fun w s = ignore (wait_status (s, false))
    in
	Concur.reset ();
	app w b
    end

    fun cm p = let
	val d = OS.FileSys.getDir ()
	val f = SrcPath.osstring p
	fun st s =
	    (Say.vsay ["(", servName s, "): project ", f, "\n"];
	     send (s, concat ["cm ", fname (d, s), " ", fname (f, s), "\n"]);
	     ignore (wait_status (s, false)))
	val l = !idle
	val _ = idle := []
	val tl = map (fn s => Concur.fork (fn () => st s)) l
    in
	app Concur.wait tl
    end

    fun cmb db = let
	val d = OS.FileSys.getDir ()
	fun st s =
	    (Say.vsay ["(", servName s, "): bootstrap compile ", db, "\n"];
	     send (s, concat ["cmb ", fname (d, s), " ", db, "\n"]);
	     ignore (wait_status (s, false)))
	val l = !idle
	val _ = idle := []
	val tl = map (fn s => Concur.fork (fn () => st s)) l
    in
	app Concur.wait tl
    end

    fun evict i = let
	val p = SmlInfo.sourcepath i
	val f = SrcPath.osstring p
	fun ev (s as ((_, _, r), _)) = r := fname (f, s) :: !r
    in
	StringMap.app ev (!all)
    end

    fun enable () = enabled := true
    fun disable () = enabled := false

    fun withServers f =
	SafeIO.perform { openIt = enable,
			 closeIt = disable,
			 work = f,
			 cleanup = fn () => () }
end
