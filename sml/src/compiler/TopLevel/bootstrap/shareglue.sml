(* Copyright 1997 by AT&T Bell Laboratories *)
(* shareglue.sml *)

(* 
 * This is the functor that builds up the interactive compiler;
 * it starts by loading the primEnv, and all stuff in the boot
 * directory. It is triggered by applying this functor to the
 * the XXVisComp structure specific to architecture XX.
 *)
functor IntShare (structure VC : VISCOMP) : sig end = 
struct
  structure BootEnv = BootEnvF(VC)

  (* environment initializations *)
  val _ = (#set VC.EnvRef.pervasive (SCEnv.SC (BootEnv.makePervEnv())))

  (* establish default signal handlers *)
  local
    fun handleINT _ = !Unsafe.topLevelCont
    fun handleTERM _ = OS.Process.exit OS.Process.failure
    fun ifSignal (sigName, handler) = 
      (case Signals.fromString sigName
	of SOME s => 
            (Signals.overrideHandler (s, Signals.HANDLER handler); ())
	 | _ => ())
  in val _ = (
       Signals.overrideHandler (Signals.sigINT, Signals.HANDLER handleINT);
       Signals.overrideHandler (Signals.sigTERM, Signals.HANDLER handleTERM);
       ifSignal ("QUIT", handleTERM))
  end

  (* launch interactive loop *)
  val _ = (
    Control.Print.say "Go for it\n";
    VC.Interact.interact())

end (* functor IntShare *)

