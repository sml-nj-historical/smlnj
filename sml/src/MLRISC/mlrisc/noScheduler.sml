(* noSchedule.sml --- do nothing with the instructions.
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor NoScheduler(Instructions : INSTRUCTIONS) : SCHEDULER =
struct
  structure I = Instructions
  fun schedule(instrs, _) = instrs
end
 


(*
 * $Log: noScheduler.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
