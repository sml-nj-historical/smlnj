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
    type server = string * Unix.proc * pathtrans

    val enabled = ref true
    val nservers = ref 0
    val all = ref (StringMap.empty: server StringMap.map)

    val idle = ref ([]: server list)
    val someIdle = ref (Concur.ucond ())

    fun fname (n, NONE) = n
      | fname (n, SOME f) = if OS.Path.isAbsolute n then f n else n

    fun send (name, outs, s) =
	(Say.dsay ["-> ", name, ": ", s];
	 TextIO.output (outs, s);
	 TextIO.flushOut outs)

    fun show_idle () =
	Say.dsay ("Idle:" ::
		  foldr (fn ((n, _, _), l) => " " :: n :: l) ["\n"] (!idle))

    (* Mark a server idle; signal all those who are currently waiting for
     * that...*)
    fun mark_idle (s as (name, _, _)) =
	(idle := s :: !idle;
	 Concur.signal (!someIdle);
	 Say.dsay ["Scheduler: slave ", name, " has become idle.\n"];
	 show_idle ())

    (* Grab an idle server; wait if necessary; reinitialize condition
     * if taking the only server. *)
    fun grab () =
	case !idle of
	    [] => (Say.dsay ["Scheduler: waiting for idle slave.\n"];
		   Concur.wait (!someIdle);
		   grab ())
	  | [only as (name, _, _)] =>
		(Say.dsay ["Scheduler: taking last idle slave (",
			   name, ").\n"];
		 idle := [];
		 someIdle := Concur.ucond ();
		 only)
	  | (first as (name, _, _)) :: more =>
		(Say.dsay ["Scheduler: taking idle slave (", name, ").\n"];
		 idle := more;
		 show_idle ();
		 first)

    fun wait_status (name, p, tr) = let
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

	fun loop () =
	    case TextIO.canInput (ins, 1) of
		NONE => wait ()
	      | SOME 0 => wait ()
	      | SOME _ => let
		    val line = TextIO.inputLine ins
		in
		    if line = "" then (crashed (); false)
		    else
			(Say.dsay ["<- ", name, ": ", line];
			 case String.tokens Char.isSpace line of
			     ["SLAVE:", "ok"] =>
				 (mark_idle (name, p, tr); true)
			   | ["SLAVE:", "error"] =>
				 (mark_idle (name, p, tr); false)
			   | "SLAVE:" :: l => (unexpected l; loop ())
			   | _ => loop ())
		end

	and wait () = (Say.dsay ["Scheduler: ", name,
				 " is waiting for slave response.\n"];
		       Concur.wait (Concur.inputReady ins); loop ())
    in
	loop ()
    end

    fun stop name = let
	val (m, s) = StringMap.remove (!all, name)
	val (_, p, _) = s
	val (ins, outs) = Unix.streamsOf p
    in
	send (name, outs, "shutdown\n");
	ignore (Unix.reap p);
	all := m;
	nservers := !nservers - 1
    end handle LibBase.NotFound => ()

    fun kill name = let
	val (m, s) = StringMap.remove (!all, name)
	val (_, p, _) = s
    in
	Unix.kill (p, Posix.Signal.kill);
	ignore (Unix.reap p);
	all := m;
	nservers := !nservers - 1
    end handle LibBase.NotFound => ()

    fun start { name, cmd, pathtrans } = let
	val _ = stop name
	val p = Unix.execute cmd
	val s = (name, p, pathtrans)
    in
	if wait_status s then
	    (all := StringMap.insert (!all, name, s);
	     nservers := 1 + !nservers;
	     true)
	else false
    end
	
    fun compile p =
	if not (!enabled) orelse !nservers = 0 then false
	else let
	    val f = SrcPath.osstring p
	    val s as (name, p, tr) = grab ()
	    val (_, outs) = Unix.streamsOf p
	in
	    Say.vsay ["(", name, "): compiling ", f, "\n"];
	    send (name, outs, concat ["compile ", fname (f, tr), "\n"]);
	    wait_status s
	end

    fun waitforall () = let
	fun busy (name, p, _) =
	    not (List.exists (fn (n', _, _) => name = n') (!idle))
	val b = List.filter busy (StringMap.listItems (!all))
	fun w s = ignore (wait_status s)
    in
	app w b
    end

    fun cm p = let
	val d = OS.FileSys.getDir ()
	val f = SrcPath.osstring p
	fun st (s as (name, p, tr)) = let
	    val (_, outs) = Unix.streamsOf p
	in
	    Say.vsay ["(", name, "): project ", f, "\n"];
	    send (name, outs, concat ["cm ", fname (d, tr), " ",
				      fname (f, tr), "\n"]);
	    ignore (wait_status s)
	end
	val _ = waitforall ()
	val l = !idle
	val _ = idle := []
	val tl = map (fn s => Concur.fork (fn () => st s)) l
    in
	app Concur.wait tl
    end

    fun cmb db = let
	val d = OS.FileSys.getDir ()
	fun st (s as (name, p, tr)) = let
	    val (_, outs) = Unix.streamsOf p
	in
	    Say.vsay ["(", name, "): bootstrap compile ", db, "\n"];
	    send (name, outs, concat ["cmb ", fname (d, tr), " ", db, "\n"]);
	    ignore (wait_status s)
	end
	val _ = waitforall ()
	val l = !idle
	val _ = idle := []
	val tl = map (fn s => Concur.fork (fn () => st s)) l
    in
	app Concur.wait tl
    end

    fun enable () = enabled := true
    fun disable () = enabled := false
end
