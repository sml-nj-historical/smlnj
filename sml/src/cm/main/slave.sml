(*
 * This module implements the slave-side of the master-slave protocol used
 * for parallel make.
 *
 *   Copyright (c) 1999 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
local
    structure DG = DependencyGraph
in
  structure Slave = struct

    fun slave { pcmode, my_archos, parse, sbtrav, make } = let

	val dbr = ref BtNames.dirbaseDefault

	fun shutdown () = OS.Process.exit OS.Process.success
	fun say_ok () = Say.say ["SLAVE: ok\n"]
	fun say_error () = Say.say ["SLAVE: error\n"]
	fun say_pong () = Say.say ["SLAVE: pong\n"]

	fun path (s, pcmode) = SrcPath.fromDescr pcmode s
		  
	fun chDir d =
	    OS.FileSys.chDir (SrcPath.osstring (path (d, pcmode)))

	fun waitForStart () = let
	    val line = TextIO.inputLine TextIO.stdIn
	in
	    if line = "" then shutdown ()
	    else case String.tokens Char.isSpace line of
		["cd", d] => (chDir d; say_ok (); waitForStart ())
	      | ["cm", archos, f] => do_cm (archos, f)
	      | ["cmb", archos, f] => do_cmb (archos, f)
	      | ["ping"] => (say_pong (); waitForStart ())
	      | ["finish"] => (say_ok (); waitForStart ())
	      | ["dirbase", db] =>
		    (say_ok (); dbr := db; waitForStart ())
	      | ["shutdown"] => shutdown ()
	      | _ => (say_error (); waitForStart ())
	end handle _ => (say_error (); waitForStart ())

	and do_cmb (archos, f) = let
	    val slave = CMBSlave.slave make
	in
	    case slave archos (!dbr, f) of
		NONE => (say_error (); waitForStart ())
	      | SOME (g, trav, cmb_pcmode) => let
		    val _ = say_ok ()
		    val index = Reachable.snodeMap g
		in
		    workLoop (index, trav, cmb_pcmode)
		end
	end handle _ => (say_error (); waitForStart ())

	and do_cm (archos, f) =
	    if archos <> my_archos then (say_error (); waitForStart ())
	    else let
		val p = path (f, pcmode)
	    in
		case parse p of
		    NONE => (say_error (); waitForStart ())
		  | SOME (g, gp) => let
			val _ = say_ok ()
			val index = Reachable.snodeMap g
			val trav = sbtrav () gp
			fun trav' sbn = isSome (trav sbn)
		    in
			workLoop (index, trav', pcmode)
		    end
	    end handle _ => (say_error (); waitForStart ())

	and workLoop (index, trav, pcmode) = let
	    fun loop () = let
		val line = TextIO.inputLine TextIO.stdIn
	    in
		if line = "" then shutdown ()
		else case String.tokens Char.isSpace line of
		    ["cd", d] => (chDir d; say_ok (); loop ())
		  | ["compile", f] => let
			val p = path (f, pcmode)
		    in
			case SrcPathMap.find (index, p) of
			    NONE => (say_error (); loop ())
			  | SOME sn => let
				val sbn = DG.SB_SNODE sn
			    in
				if trav sbn then (say_ok (); loop ())
				else (say_error (); loop ())
			    end
		    end
		  | ["cm", archos, f] => do_cm (archos, f)
		  | ["cmb", archos, f] => do_cmb (archos, f)
		  | ["finish"] => (say_ok (); waitForStart ())
		  | ["dirbase", db] =>
		    (say_ok (); dbr := db; waitForStart ())
		  | ["ping"] => (say_pong (); loop ())
		  | ["shutdown"] => shutdown ()
		  | _ => (say_error (); loop ())
	    end handle _ => (say_error (); loop ())
	in
	    loop ()
	end
    in
	ignore (Signals.setHandler (Signals.sigINT, Signals.IGNORE));
	say_ok ();		(* announce readiness *)
	waitForStart () handle _ => ();
	    OS.Process.exit OS.Process.failure
    end
  end
end
