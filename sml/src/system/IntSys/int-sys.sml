(* Copyright 1997 by AT&T Bell Laboratories *)
(* Copyright 1998 by Lucent Technologies *)
(* Copyright 1999 by Lucent Technologies *)
(* int-sys.sml *)

(* 
 * This is the interactive system;
 * At link-time (i.e., at bootstrap time) this code builds the boot
 * environments, sets default signal handlers, and then dumps a heap.
 * When the heap image restarts, the system goes interactive.
 * 
 * (We do not want to go interactive before dumping the heap because it
 * would mean that environments get loaded unnecessarily.)
 *
 * This code refers directly to structure Compiler, because by the time it
 * gets compiled, CM's conditional compilation facility has already
 * made sure that structure Compiler refers to the visible compiler
 * for the current architecture. 
 *)
structure InteractiveSystem : sig end = struct

    (* first, we have to step back out of the boot directory... *)
    val bootdir = OS.FileSys.getDir ()
    val _ = OS.FileSys.chDir OS.Path.parentArc
    
    (* environment initializations *)
    val { heapfile, procCmdLine } = BootEnv.init bootdir
	
    (* establish default signal handlers *)
    fun handleINT _ = !Unsafe.topLevelCont
    fun handleTERM _ = OS.Process.exit OS.Process.failure
    fun ifSignal (sigName, handler) = 
	(case Signals.fromString sigName of
	     SOME s =>
		 (Signals.overrideHandler (s, Signals.HANDLER handler); ())
	   | _ => ())

    val _ =
	(Signals.overrideHandler (Signals.sigINT, Signals.HANDLER handleINT);
	 Signals.overrideHandler (Signals.sigTERM, Signals.HANDLER handleTERM);
	 ifSignal ("QUIT", handleTERM))

    val _ = UseHook.useHook := Compiler.Interact.useFile

    (* launch interactive loop *)
    val _ = (Compiler.Control.Print.say "Generating heap image...\n";
	     if SMLofNJ.exportML heapfile then
		 (SMLofNJ.Internals.resetTimers ();
		  Compiler.Stats.reset ();
		  print Compiler.banner;
		  print "\n";
		  getOpt (procCmdLine, fn () => ()) ();
		  Compiler.Interact.interact ())
	     else
		 (print "This is...\n";
		  print Compiler.banner;
		  print "\n";
		  OS.Process.exit OS.Process.success))
end
