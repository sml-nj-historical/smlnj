(* COPYRIGHT (c) 1995 by Bell Laboratories *)
(* symenv.sml *)

structure SymbolicEnv: SYMENV = 
struct
  type flint = FLINT.prog
  type pid = PersStamps.persstamp
  type symenv = flint PersMap.map

  val empty = PersMap.empty
  fun look e p = PersMap.find(e, p)
  fun bind (p, l, e) = PersMap.insert (e, p, l)
  fun atop (e1, e2) = PersMap.unionWith #1 (e1, e2)
  fun rmv (key, map) = 
      let val (newMap, _) = PersMap.remove(map, key) 
      in newMap
      end handle e => map
  fun remove (pl, e) =  foldr rmv e pl
  fun consolidate e = e
  fun singleton (p, l) = bind (p, l, empty)
  fun listItemsi e = PersMap.listItemsi e
  fun fromListi il = foldl PersMap.insert' empty il
  fun mk (NONE, _) = empty
    | mk (_, NONE) = empty
    | mk (SOME p, SOME l) = singleton (p, l)
end (* structure SymbolicEnv *)
