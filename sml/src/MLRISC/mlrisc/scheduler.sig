(* schedule.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature SCHEDULER = 
  sig 
    structure I : INSTRUCTIONS
    val schedule : 
      (I.instruction list * int Intmap.intmap) -> I.instruction list
 end


(*
 * $Log: scheduler.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
