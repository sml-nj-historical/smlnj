(* symenv.sml
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories
 *)

structure SymbolicEnv: SYMENV = struct

    type lambda = Lambda.lexp
    type pid = PersStamps.persstamp
    type symenv = lambda PersMap.map

    val empty = PersMap.empty

    fun look e p =
	SOME (PersMap.lookup e p) handle PersMap.MapF => NONE

    fun bind (p, l, e) = PersMap.add (e, p, l)

    fun atop (e1, e2) = PersMap.overlay (e1, e2)

    fun remove (pl, e) = foldr PersMap.delete e pl

    fun consolidate e = e

    fun singleton (p, l) = bind (p, l, empty)

end

(*
 * $Log: symenv.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:37  george
 *   Version 109.24
 *
 *)
