signature PG_OPS = sig

    type ('lib, 'env, 'sym, 'syms, 'export, 'misc) context

    val SYM : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
	      -> string -> string
	      -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'sym
    val IMPORT : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		 -> 'lib -> 'syms
		 -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val COMPILE : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		  -> string -> 'env -> 'syms
		  -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val NCOMPILE : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		   -> string -> 'env -> 'syms
		   -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val MERGE : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		-> 'env list
		-> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val FILTER : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
		 -> 'env -> 'syms
		 -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'env
    val SYMS : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
	       -> 'sym list
	       -> ('lib, 'env, 'sym, 'syms, 'export, 'misc) context * 'syms
    val EXPORT : ('lib, 'env, 'sym, 'syms, 'export, 'misc) context
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
    fun SYM C ns s = generic C #Sym (ns, s)
    fun IMPORT C l ss = generic C #Imp (l, ss)
    fun COMPILE C s e ss = generic C #Com (s, e, ss, false)
    fun NCOMPILE C s e ss = generic C #Com (s, e, ss, true)
    fun MERGE C el = generic C #Mer el
    fun FILTER C e ss = generic C #Fil (e, ss)
    fun SYMS C sl = generic C #Syms sl
    fun EXPORT { Ops = { Sym, Imp, Com, Mer, Fil, Syms, Exp }, Misc } e =
	Exp Misc e
    end
end
