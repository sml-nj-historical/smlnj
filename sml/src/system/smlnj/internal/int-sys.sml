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
	structure CR = ControlRegistry

	val priority = [10, 3]
	val obscurity = 3
	val prefix = "mlrisc"

	val registry = CR.new { help = "MLRISC" }

	val _ = BasicControl.nest (prefix, registry)

	fun uc #"-" = #"_"
	  | uc c = Char.toUpper c
	fun en n = SOME ("MLRISC_" ^ String.map uc n)

	fun reg0 en c { cell, descr, stem } = let
	    val ctl = C.control { name = stem,
				  pri = priority,
				  obscurity = obscurity,
				  help = descr,
				  ctl = cell }
	in
	    CR.register registry { ctl = C.stringControl c ctl,
				   envName = en stem }
	end

	fun reg x = reg0 en x
	fun reg' x = reg0 (fn _ => NONE) x

	val int_cvt = { tyName = "int",
			fromString = Int.fromString,
			toString = Int.toString }
	val flag_cvt = { tyName = "bool",
			 fromString = Bool.fromString,
			 toString = Bool.toString }
	val real_cvt = { tyName = "real",
			 fromString = Real.fromString,
			 toString = Real.toString }
	val string_cvt = { tyName = "string",
			   fromString = SOME,
			   toString = fn x => x }
	val stringList_cvt = { tyName = "string list",
			       fromString = SOME o String.tokens Char.isSpace,
			       toString = concat o
			                foldr (fn (s, r) => " " :: s :: r) [] }
	val timing_cvt =
	    { tyName = "timing",
	      fromString = fn _ => (NONE : Control.MLRISC.cpu_time option),
	      toString = fn _ => "<timing>" }
    in
	val _ = app (reg' int_cvt) (!Control.MLRISC.counters)
	val _ = app (reg int_cvt) (!Control.MLRISC.ints)
	val _ = app (reg flag_cvt) (!Control.MLRISC.flags)
	val _ = app (reg real_cvt) (!Control.MLRISC.reals)
	val _ = app (reg string_cvt) (!Control.MLRISC.strings)
	val _ = app (reg stringList_cvt) (!Control.MLRISC.stringLists)
	val _ = app (reg' timing_cvt) (!Control.MLRISC.timings)
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

    (* initialize control *)
    val _ = ControlRegistry.init BasicControl.topregistry

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
