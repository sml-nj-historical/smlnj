(* Copyright 1997 by AT&T Bell Laboratories *)
(* Copyright 1998 by Lucent Technologies *)
(* int-sys.sml *)

(* 
 * This is the interactive system;
 * At link-time (i.e., at bootstrap time) this code builds the boot
 * environments, sets default signal handlers, and then goes interactive.
 *
 * This code refers directly to structure Compiler, because by the time it
 * gets compiled, CM's conditional compilation facility has already
 * made sure that structure Compiler refers to the visible compiler
 * for the current architecture. 
 *)
structure InteractiveSystem : sig end = struct

    (* environment initializations *)
    val pervasive = BootEnv.bootEnv ()
    val _ = #set Compiler.EnvRef.pervasive (Compiler.CoerceEnv.b2e pervasive)
	
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

    (* launch interactive loop *)
    val _ = (Compiler.Control.Print.say "Go for it\n";
	     Compiler.Interact.interact())
end

(*
 * $Log$
 *)
