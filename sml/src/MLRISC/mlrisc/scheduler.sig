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
 * $Log$
 *)
