(* dynenv.sml
 *
 * Copyright 1996 by AT&T Bell Laboratories.
 *
 *)

structure PersMap = MapF (struct
    type elem=PersStamps.persstamp
    fun pid1 < pid2 = (PersStamps.compare(pid1, pid2) = LESS)
  end)

structure DynamicEnv : DYNENV =
struct

  type pid = PersStamps.persstamp

  structure Map = PersMap

  type object = Unsafe.Object.object

  datatype dynenv = NORM of object Map.map * dynenv
                  | SPECIAL of (pid -> object) * dynenv
		  | EMPTY
  (* chain invariant: only one NORM in a row. *)

  exception Unbound = Map.MapF
  exception SpecialEnv

  val empty = EMPTY

  fun special (f,next) = SPECIAL(f,next)

  fun look (NORM(map,next)) pid = ((Map.lookup map pid)
				  handle Unbound => look next pid)
    | look (SPECIAL(f,next)) pid = ((f pid) handle Unbound => look next pid)
    | look EMPTY pid = raise Unbound

  fun bind (pid,binding,NORM(map,next)) = NORM(Map.add(map,pid,binding),next)
    | bind (pid,binding,x) = NORM(Map.add(Map.empty,pid,binding),x)

  fun atop(NORM(topmap,EMPTY),NORM(bottommap,next)) = 
        NORM(Map.overlay(topmap,bottommap),next)
    | atop(NORM(topmap,EMPTY),bottom) = NORM(topmap,bottom)
    | atop(NORM(topmap,nexttop),bottom) = NORM(topmap,atop(nexttop,bottom))
    | atop(SPECIAL(f,nexttop),bottom) = SPECIAL(f,atop(nexttop,bottom))
    | atop(EMPTY,bottom) = bottom
       
  fun remove(pids: pid list, NORM(map,next)) =
        NORM(foldr Map.delete map pids, remove(pids,next))
    | remove(pids,SPECIAL(f,next)) = raise SpecialEnv
    | remove(pids,EMPTY) = EMPTY
      
  fun consolidate e = e

  fun singleton (p, v) = bind (p, v, empty)

end (* structure DynamicEnv *)

(*
 * $Log: dynenv.sml,v $
 * Revision 1.2  1997/06/30  19:37:23  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:36  george
 *   Version 109.24
 *
 *)
