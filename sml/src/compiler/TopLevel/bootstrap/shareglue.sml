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
  val _ = (#set VC.EnvRef.pervasive (CMEnv.CM (BootEnv.makePervEnv())))

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


(*
 * $Log: shareglue.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
