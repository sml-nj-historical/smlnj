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
	val b_gs = { get = fn () => false, set = fn (x: bool) => () }
	val i_gs = { get = fn () => 0, set = fn (x: int) => () }
	fun s_iogs (s: string) =
	    { get = fn () => SOME 0, set = fn (x: int option) => () }
	fun s_u (s: string) = ()
	fun s's_u (s1: string, s2: string) = ()

	(* the hook itself *)
	val hook = ref { stabilize = b's_b,
			 recomp = s_b,
			 make = s_b,
			 autoload = s_b,
			 reset = u_u,
			 verbose = b_gs,
			 debug = b_gs,
			 keep_going = b_gs,
			 warn_obsolete = b_gs,
			 parse_caching = i_gs,
			 setAnchor = s's_u,
			 cancelAnchor = s_u,
			 resetPathConfig = u_u,
			 synchronize = u_u,
			 showPending = u_u,
			 listLibs = u_u,
			 dismissLib = s_u,
			 symval = s_iogs }

	fun gs label = let
	    fun get' () = let
		val { get, set } = label (!hook)
	    in
		get ()
	    end
	    fun set' x = let
		val { get, set } = label (!hook)
	    in
		set x
	    end
	in
	    { get = get', set = set' }
	end
    in
	(* the routine to be called at bootstrap time... *)
	fun init v = hook := v

	local
	in
	    (* the CM structure that will be visible at top-level *)
	    structure CM = struct
		fun stabilize b s = #stabilize (!hook) b s
		fun recomp s = #recomp (!hook) s
		fun make s = #make (!hook) s
		fun autoload s = #autoload (!hook) s
		fun reset () = #reset (!hook) ()
		val verbose = gs #verbose
		val debug = gs #debug
		val keep_going = gs #keep_going
		val warn_obsolete = gs #warn_obsolete
		val parse_caching = gs #parse_caching
		fun setAnchor (a, s) = #setAnchor (!hook) (a, s)
		fun cancelAnchor a = #cancelAnchor (!hook) a
		fun resetPathConfig () = #resetPathConfig (!hook) ()
		fun synchronize () = #synchronize (!hook) ()
		fun showPending () = #showPending (!hook) ()
		fun listLibs () = #listLibs (!hook) ()
		fun dismissLib l = #dismissLib (!hook) l
		fun symval s = #symval (!hook) s
	    end
	end
    end
end
