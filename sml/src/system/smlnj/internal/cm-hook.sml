(*
 * Hook module for CM.
 *   This module exists to break the static dependency between CM and
 *   the compiler.  This way we avoid lengthy waits for the autoloader
 *   when touching CM at the interactive toplevel.
 *   (CM is there and running at bootstrap time, so it can easily
 *    install itself into the hook if this is what's desired.)
 *
 *   Copyright (c) 1999 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure CmHook = struct
    local
	(* some dummy routines to make up the initial contents of the hook *)
	fun b's_b (b: bool) (s: string) = false
	fun s_b (s: string) = false

	(* the hook itself *)
	val hook = ref { stabilize = b's_b,
			 recomp = s_b,
			 make = s_b,
			 autoload = s_b }
    in
	(* the routine to be called at bootstrap time... *)
	fun init v = hook := v

	(* the CM structure that will be visible at top-level *)
	structure CM :> MINIMAL_CM = struct
	    fun autoload s = #autoload (!hook) s
	    fun make s = #make (!hook) s
	    fun recomp s = #recomp (!hook) s
	    fun stabilize b s = #stabilize (!hook) b s
	end
    end
end
