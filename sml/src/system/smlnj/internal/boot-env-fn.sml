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
		  val cminit : string * DynamicEnv.dynenv * envrequest ->
		               (unit -> unit) option
		  val cmbmake: string -> unit) :> BOOTENV = struct

    exception BootFailure

    (* To be able to use ml-yacc and ml-lex at -rebuild time it is necessary
     * to force their plugins to be _always_ plugged in.  We achieve this
     * by simply mentioning the structure names here. *)
    structure YaccTool = YaccTool and LexTool = LexTool

    structure DynE = DynamicEnv
    structure Print = GenericVC.Control.Print

    fun say s = (Print.say s; Print.flush ())
    fun die s = (say s; raise BootFailure)

    (* just run CMB.make to make a new set of binfiles... *)
    fun recompile bindir =
	(say (concat ["[building new binfiles in ", bindir, "]\n"]);
	 cmbmake bindir;
	 OS.Process.exit OS.Process.success)

    local
	structure U = Unsafe
    in
	fun initialize (bootdir, er) = let
	    fun mkDE (U.NILrde, de) = de
	      | mkDE (U.CONSrde (rawdynpid, obj, rest), de) = let
		    val dynpid = GenericVC.PersStamps.fromBytes rawdynpid
		in
		    mkDE (rest, DynE.bind (dynpid, obj, de))
		end
	    val de = mkDE (!U.pStruct, DynE.empty)
	in
	    U.pStruct := U.NILrde;
	    cminit (bootdir, de, er)
	end
    end

    fun init bootdir = let
	(* grab relevant command line arguments... *)
	fun vArg (prefix, arg) =
	    if String.isPrefix prefix arg then
		SOME (String.extract (arg, size prefix, NONE))
	    else NONE
	fun bootArgs ([], newbindir, heapfile, er) = (newbindir, heapfile, er)
	  | bootArgs ("@SMLbare" :: rest, newbindir, heapfile, _) =
	    bootArgs (rest, newbindir, heapfile, BARE)
	  | bootArgs (head :: rest, newbindir, heapfile, er) =
	    (case vArg ("@SMLrebuild=", head) of
		 nbd as SOME _ => bootArgs (rest, nbd, heapfile, er)
	       | NONE =>
		     (case vArg ("@SMLheap=", head) of
			  SOME hf => bootArgs (rest, newbindir, hf, er)
			| NONE => bootArgs (rest, newbindir, heapfile, er)))

	val (newbindir, heapfile, er) =
	    bootArgs (SMLofNJ.getAllArgs (),
		      NONE,
		      "sml." ^ architecture,
		      AUTOLOAD)
	val newbindir = Option.map OS.Path.mkCanonical newbindir
    in
	case newbindir of
	    NONE => let
		val procCmdLine = initialize (bootdir, er)
	    in
		{ heapfile = heapfile, procCmdLine = procCmdLine }
	    end
	  | SOME nbd =>
		if nbd = bootdir then
		    die "@SMLboot= and @SMLrebuild= name the same directory\n"
		else recompile nbd
    end
end
