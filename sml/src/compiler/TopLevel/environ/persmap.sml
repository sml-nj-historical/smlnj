(* Copyright 1996 by AT&T Bell Laboratories. *)
(* persmap.sml *)

structure PersMap : MAPF = 
  MapF (struct
          type elem=PersStamps.persstamp
          fun pid1 < pid2 = (PersStamps.compare(pid1, pid2) = LESS)
        end)


(*
 * $Log: persmap.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
