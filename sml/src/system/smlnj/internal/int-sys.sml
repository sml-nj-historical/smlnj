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

    val _ = UseHook.useHook := Backend.Interact.useFile

    local
	(* register the MLRISC controls with the central controls
	 * facility... *)
	structure C = Controls

	val m0 = C.noconfig
	val m = C.registry { name = "MLRISC",
			     priority = [10, 3],
			     obscurity = 3,
			     prefix = "mlrisc-",
			     default_suffix = SOME "-default",
			     mk_ename = NONE }

	val counter_r = C.group m0 C.int
	val int_r =	C.group m C.int 
	val flag_r = C.group m C.bool
	val real_r = C.group m C.real
	val string_r = C.group m C.string
	val stringList_r = C.group m C.stringList
	val timing_r = C.group m0
	      { tname = "timing",
		fromString = fn _ => (NONE : Control.MLRISC.cpu_time option),
		toString = fn _ => "<timing>" }
    in
	val _ = app (C.reg counter_r) (!Control.MLRISC.counters)
	val _ = app (C.reg int_r) (!Control.MLRISC.ints)
	val _ = app (C.reg flag_r) (!Control.MLRISC.flags)
	val _ = app (C.reg real_r) (!Control.MLRISC.reals)
	val _ = app (C.reg string_r) (!Control.MLRISC.strings)
	val _ = app (C.reg stringList_r) (!Control.MLRISC.stringLists)
	val _ = app (C.reg timing_r) (!Control.MLRISC.timings)
    end

    (* add cleanup code that resets the internal timers and stats
     * when resuming from exportML... *)
    local
	structure I = SMLofNJ.Internals
	structure C = I.CleanUp
	fun reset _ = (I.resetTimers (); Stats.reset ())
    in
        val _ = C.addCleaner ("initialize-timers-and-stats", [C.AtInit], reset)
    end

    (* launch interactive loop *)
    val _ = (Control.Print.say "Generating heap image...\n";
	     if SMLofNJ.exportML heapfile then
		 (print CompilerVersion.banner;
		  print "\n";
		  getOpt (procCmdLine, fn () => ()) ();
		  Backend.Interact.interact ())
	     else
		 (print "This is...\n";
		  print CompilerVersion.banner;
		  print "\n";
		  OS.Process.exit OS.Process.success))
end
