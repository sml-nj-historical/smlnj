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
	fun u_u () = ()
	fun bo_b (bo: bool option) = false
	fun io_i (io: int option) = 0
	fun s_u (s: string) = ()
	fun s's_u (s1: string, s2: string) = ()

	(* the hook itself *)
	val hook = ref { stabilize = b's_b,
			 recomp = s_b,
			 make = s_b,
			 autoload = s_b,
			 reset = u_u,
			 verbose = bo_b,
			 debug = bo_b,
			 keep_going = bo_b,
			 parse_caching = io_i,
			 setAnchor = s's_u,
			 cancelAnchor = s_u,
			 resetPathConfig = u_u,
			 synchronize = u_u,
			 showPending = u_u }
    in
	(* the routine to be called at bootstrap time... *)
	fun init v = hook := v

	(* the CM structure that will be visible at top-level *)
	structure CM = struct
	    fun stabilize b s = #stabilize (!hook) b s
	    fun recomp s = #recomp (!hook) s
	    fun make s = #make (!hook) s
	    fun autoload s = #autoload (!hook) s
	    fun reset () = #reset (!hook) ()
	    fun verbose bo = #verbose (!hook) bo
	    fun debug bo = #debug (!hook) bo
	    fun keep_going bo = #keep_going (!hook) bo
	    fun parse_caching io = #parse_caching (!hook) io
	    fun setAnchor (a, s) = #setAnchor (!hook) (a, s)
	    fun cancelAnchor a = #cancelAnchor (!hook) a
	    fun resetPathConfig () = #resetPathConfig (!hook) ()
	    fun synchronize () = #synchronize (!hook) ()
	    fun showPending () = #showPending (!hook) ()
	end
    end
end
