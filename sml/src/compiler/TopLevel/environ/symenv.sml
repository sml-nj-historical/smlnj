(* COPYRIGHT (c) 1995 by Bell Laboratories *)
(* symenv.sml *)

structure SymbolicEnv: SYMENV = 
struct
  type flint = CompBasic.flint
  type pid = PersStamps.persstamp
  type symenv = flint PersMap.map

  val empty = PersMap.empty
  fun look e p = SOME (PersMap.lookup e p) handle PersMap.MapF => NONE
  fun bind (p, l, e) = PersMap.add (e, p, l)
  fun atop (e1, e2) = PersMap.overlay (e1, e2)
  fun remove (pl, e) = foldr PersMap.delete e pl
  fun consolidate e = e
  fun singleton (p, l) = bind (p, l, empty)

end (* structure SymbolicEnv *)


(*
 * $Log: symenv.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
