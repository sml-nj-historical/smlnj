structure Servers :> SERVERS = struct
    type server = string * Unix.proc

    val nservers = ref 0
    val all = ref ([]: server list)

    val idle = ref ([]: server list)
    val someIdle = ref (Concur.ucond ())

    fun send (name, outs, s) =
	(Say.say ["-> ", name, ": ", s];
	 TextIO.output (outs, s);
	 TextIO.flushOut outs)

    fun show_idle () =
	Say.say ("Idle:" ::
		 foldr (fn ((n, _), l) => " " :: n :: l) ["\n"] (!idle))

    (* Mark a server idle; signal all those who are currently waiting for
     * that...*)
    fun mark_idle (s as (name, _)) =
	(idle := s :: !idle;
	 Concur.signal (!someIdle);
	 Say.say ["Scheduler: ", name, " has become idle.\n"];
	 show_idle ())

    (* Grab an idle server; wait if necessary; reinitialize condition
     * if taking the only server. *)
    fun grab () =
	case !idle of
	    [] => (Say.say ["Scheduler: waiting for idle server.\n"];
		   Concur.wait (!someIdle);
		   grab ())
	  | [only as (name, _)] =>
		(Say.say ["Scheduler: taking last idle server (",
			  name, ").\n"];
		 idle := [];
		 someIdle := Concur.ucond ();
		 only)
	  | (first as (name, _)) :: more =>
		(Say.say ["Scheduler: taking idle server (", name, ").\n"];
		 show_idle ();
		 idle := more; first)

    fun wait_status (name, p) = let
	val (ins, _) = Unix.streamsOf p

	fun unexpected l = let
	    fun word (w, l) = " " :: w :: l
	in
	    Say.say ("! Unexpected response from compile server " ::
		     name :: ":" :: foldr word ["\n"] l)
	end
	     
	fun crashed () =
	    (Say.say ["! Compile server ", name, " has crashed\n"];
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
			(Say.say ["<- ", name, ": ", line];
			 case String.tokens Char.isSpace line of
			     ["SLAVE:", "ok"] =>
				 (mark_idle (name, p); true)
			   | ["SLAVE:", "error"] =>
				 (mark_idle (name, p); false)
			   | "SLAVE:" :: l => (unexpected l; loop ())
			   | _ => loop ())
		end

	and wait () = (Say.say ["Scheduler: ", name,
				" is waiting for server response.\n"];
		       Concur.wait (Concur.inputReady ins); loop ())
    in
	loop ()
    end

    fun add { name, cmd } = let
	val p = Unix.execute cmd
	val s = (name, p)
    in
	ignore (wait_status s);
	all := s :: !all;
	nservers := 1 + !nservers
    end

    fun compile p =
	if !nservers = 0 then false
	else let
	    val f = SrcPath.osstring p
	    val s as (name, p) = grab ()
	    val (_, outs) = Unix.streamsOf p
	in
	    Say.say ["(", name, "): compiling ", f, "\n"];
	    send (name, outs, concat ["compile ", f, "\n"]);
	    wait_status s
	end

    fun waitforall () = let
	fun busy (name, p) =
	    not (List.exists (fn (n', _) => name = n') (!idle))
	val b = List.filter busy (!all)
	fun w s = ignore (wait_status s)
    in
	app w b
    end

    fun start (c, p) = let
	val d = OS.FileSys.getDir ()
	val f = SrcPath.osstring p
	fun st (s as (name, p)) = let
	    val (_, outs) = Unix.streamsOf p
	in
	    Say.say ["(", name, "): starting ", f, "\n"];
	    send (name, outs, concat ["cm ", d, " ", f, "\n"]);
	    ignore (wait_status s)
	end
	val _ = waitforall ()
	val l = !idle
	val _ = idle := []
    in
	app st l
    end
end
