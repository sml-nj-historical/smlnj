(* Copyright 1998 by Lucent Technologies *)
(* boot-env.sml *)

(* Run the BootEnvF functor which builds the boot environments.
 *   It is important that this functor is done executing by the time
 *   the code for the InteractiveSystem runs.  Otherwise we would never
 *   be able to get rid of CM/CMB from an interactive heap image.
 *  -M.Blume (6/1998)
 *)
structure BootEnv =
    BootEnvF (structure BF = Compiler.Binfile
	      val architecture = Compiler.architecture
	      val setRetargetPervStatEnv = CMB.setRetargetPervStatEnv
	      fun cmbmake bindir = (CMB.make' (SOME bindir);
				    CMB.wipeOut ()))
