(*
 * Copyright 1996 by Bell Laboratories
 *  boot.sml -- bootstrap environments
 *
 *   completely redone by M.Blume (5/1998)
 *   ... and again in the course of switching over to the new CM
 *       (M. Blume, 7/1999)
 *)
signature BOOTENV = sig
    val init: unit -> string
end

functor BootEnvF (datatype envrequest = AUTOLOAD | BARE
		  val architecture: string
		  val cminit : string * DynamicEnv.dynenv * envrequest -> unit
		  val cmbmake: string -> unit) :> BOOTENV = struct

    exception BootFailure

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

    fun init () = let
	(* grab relevant command line arguments... *)
	fun vArg (prefix, arg) =
	    if String.isPrefix prefix arg then
		SOME (String.extract (arg, size prefix, NONE))
	    else NONE
	fun bootArgs ([], bootdir, newbindir, heapfile, er) =
	    (bootdir, newbindir, heapfile, er)
	  | bootArgs ("@SMLbare" :: rest, bootdir, newbindir, heapfile, _) =
	    bootArgs (rest, bootdir, newbindir, heapfile, BARE)
	  | bootArgs (head :: rest, bootdir, newbindir, heapfile, er) =
	    (case vArg ("@SMLboot=", head) of
		 SOME bootdir =>
		     bootArgs (rest, bootdir, newbindir, heapfile, er)
	       | NONE =>
		     (case vArg ("@SMLrebuild=", head) of
			  newbindir as SOME _ =>
			      bootArgs (rest, bootdir, newbindir, heapfile, er)
			| NONE =>
			      (case vArg ("@SMLheap=", head) of
				   SOME heapfile =>
				       bootArgs (rest, bootdir, newbindir,
						 heapfile, er)
				 | NONE =>
				       bootArgs (rest, bootdir, newbindir,
						 heapfile, er))))

	val (bootdir, newbindir, heapfile, er) =
	    bootArgs (SMLofNJ.getAllArgs (),
		      "comp.boot." ^ architecture,
		      NONE,
		      "sml." ^ architecture,
		      AUTOLOAD)
	val bootdir = OS.Path.mkCanonical bootdir
	val newbindir = Option.map OS.Path.mkCanonical newbindir
    in
	case newbindir of
	    NONE => (initialize (bootdir, er); heapfile)
	  | SOME nbd =>
		if nbd = bootdir then
		    die "@SMLboot= and @SMLrebuild= name the same directory\n"
		else recompile nbd
    end
end
