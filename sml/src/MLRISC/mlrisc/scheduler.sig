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
 * Revision 1.3  1997/09/17 17:15:10  george
 * *** empty log message ***
 *
# Revision 1.2  1997/07/17  12:32:33  george
#   The regmap is now represented as an int map rather than using arrays.
#
# Revision 1.1.1.1  1997/04/19  18:14:21  george
#   Version 109.27
#
 *)
