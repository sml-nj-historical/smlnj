(* generic-ops.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
signature PG_OPS = sig

    type ('lib, 'env, 'sym, 'syms, 'export, 'misc) context

    val sym : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
	      -> string -> string
	      -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'sym
    val syms : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
	       -> 'sym list
	       -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'syms
    val import : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		 -> 'lib -> 'syms
		 -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val compile : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		  -> string -> 'env -> 'syms
		  -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val ncompile : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		   -> string -> 'env -> 'syms
		   -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val merge : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		-> 'env list
		-> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val filter : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		 -> 'env -> 'syms
		 -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val export : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		 -> 'env
		 -> 'export
end

structure PGOps : PG_OPS = struct

    type ('lib, 'env, 'sym, 'syms, 'export, 'misc) context =
	 { Ops : { Sym: 'misc -> string * string -> 'misc * 'sym,
		   Imp: 'misc -> 'lib * 'syms -> 'misc * 'env,
		   Com: 'misc -> string * 'env * 'syms * bool -> 'misc * 'env,
		   Mer: 'misc -> 'env list -> 'misc * 'env,
		   Fil: 'misc -> 'env * 'syms -> 'misc * 'env,
		   Syms: 'misc -> 'sym list -> 'misc * 'syms,
		   Exp: 'misc -> 'env -> 'export },
	   Misc: 'misc }

    local
	fun generic { Ops = Ops as { Sym, Imp, Com, Mer, Fil, Syms, Exp },
		      Misc }
		    sel args =
	    let val (Misc', res) = sel Ops Misc args
	    in ({ Ops = Ops, Misc = Misc' }, res)
	    end
    in
    fun sym c ns s = generic c #Sym (ns, s)
    fun syms c sl = generic c #Syms sl
    fun import c l ss = generic c #Imp (l, ss)
    fun compile c s e ss = generic c #Com (s, e, ss, false)
    fun ncompile c s e ss = generic c #Com (s, e, ss, true)
    fun merge c el = generic c #Mer el
    fun filter c e ss = generic c #Fil (e, ss)
    fun export { Ops = { Sym, Imp, Com, Mer, Fil, Syms, Exp }, Misc } e =
	Exp Misc e
    end
end
