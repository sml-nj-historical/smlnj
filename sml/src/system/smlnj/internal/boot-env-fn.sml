(*
 * Copyright 1996 by Bell Laboratories
 *  boot.sml -- bootstrap environments
 *
 *   completely redone by M.Blume (5/1998)
 *   ... and again in the course of switching over to the new CM
 *       (M. Blume, 7/1999)
 *)
signature BOOTENV = sig
    val init:
	string -> { heapfile: string, procCmdLine: (unit -> unit) option }
end

functor BootEnvF (datatype envrequest = AUTOLOAD | BARE
		  val architecture: string
		  val cminit : string * DynamicEnv.env * envrequest
			       * (TextIO.instream -> unit)(* useStream *)
			       * (string -> unit) (* useFile *)
			       * ((Ast.dec * EnvRef.envref -> unit) -> unit)
			       -> (unit -> unit) option
		  val cmbmake: string * bool -> unit) :> BOOTENV = struct

    exception BootFailure

    (* To be able to use ml-yacc and ml-lex at -rebuild time it is necessary
     * to force their plugins to be _always_ plugged in.  We achieve this
     * by simply mentioning the structure names here. *)
    structure YaccTool = YaccTool and LexTool = LexTool
    (* The classifier for dir-tool.cm must also be registered permanently... *)
    structure DirToolClassify = DirToolClassify

    structure DynE = DynamicEnv
    structure Print = Control.Print

    fun say s = (Print.say s; Print.flush ())
    fun die s = (say s; raise BootFailure)

    (* just run CMB.make to make a new set of binfiles... *)
    fun recompile (bindir, light) =
	(say (concat ["[building new binfiles in ", bindir, "]\n"]);
	 cmbmake (bindir, light);
	 OS.Process.exit OS.Process.success)

    local
	structure U = Unsafe
    in
	fun initialize (bootdir, er) = let
	    fun mkDE (U.NILrde, de) = de
	      | mkDE (U.CONSrde (rawdynpid, obj, rest), de) = let
		    val dynpid = PersStamps.fromBytes rawdynpid
		in
		    mkDE (rest, DynE.bind (dynpid, obj, de))
		end
	    val de = mkDE (!U.pStruct, DynE.empty)
	in
	    U.pStruct := U.NILrde;
	    cminit (bootdir, de, er,
		    Backend.Interact.useStream,
		    Backend.Interact.useFile,
		    Backend.Interact.installCompManager)
	end
    end

    fun init bootdir = let
	(* grab relevant command line arguments... *)
	fun caseArg arg cases dfl = let
	    fun loop [] = dfl ()
	      | loop ({ prefix, action } :: l) =
		if String.isPrefix prefix arg then
		    action (String.extract (arg, size prefix, NONE))
		else loop l
	in
	    loop cases
	end

	fun bootArgs ([], newbindir, heapfile, er) = (newbindir, heapfile, er)
	  | bootArgs ("@SMLbare" :: rest, newbindir, heapfile, _) =
	    bootArgs (rest, newbindir, heapfile, BARE)
	  | bootArgs (head :: rest, newbindir, heapfile, er) =
	    caseArg head
		    [{ prefix = "@SMLheap=",
		       action = fn hf => bootArgs (rest, newbindir, hf, er) },
		     { prefix = "@SMLrebuild=",
		       action = fn nbd =>
				   bootArgs (rest, SOME (nbd, false),
					     heapfile, er) },
		     { prefix = "@SMLlightrebuild=",
		       action = fn nbd =>
				   bootArgs (rest, SOME (nbd, true),
					     heapfile, er) }]
		    (fn () => bootArgs (rest, newbindir, heapfile, er))

	val (newbindir, heapfile, er) =
	    bootArgs (SMLofNJ.getAllArgs (),
		      NONE,
		      "sml." ^ architecture,
		      AUTOLOAD)
    in
	case newbindir of
	    NONE => let
		val procCmdLine = initialize (bootdir, er)
	    in
		{ heapfile = heapfile, procCmdLine = procCmdLine }
	    end
	  | SOME (nbd, light) => let
		val nbd = OS.Path.mkCanonical nbd
	    in
		if nbd = bootdir then
		    die "@SMLboot= and @SMLrebuild= name the same directory\n"
		else recompile (nbd, light)
	    end
    end
end
