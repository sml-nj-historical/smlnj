(* Copyright 1996 by AT&T Bell Laboratories. *)
(* persmap.sml *)

structure PersMap : ORD_MAP = 
  BinaryMapFn
    (struct
       type ord_key = PersStamps.persstamp
       val compare = PersStamps.compare
     end)


(*
 * $Log: persmap.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
