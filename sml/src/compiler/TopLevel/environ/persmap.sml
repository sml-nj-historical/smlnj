(* Copyright 1996 by AT&T Bell Laboratories. *)
(* persmap.sml *)

structure PersMap : MAPF = 
  MapF (struct
          type elem=PersStamps.persstamp
          fun pid1 < pid2 = (PersStamps.compare(pid1, pid2) = LESS)
        end)

