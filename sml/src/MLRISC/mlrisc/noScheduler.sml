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
 * Revision 1.1.1.1  1997/04/19 18:14:21  george
 *   Version 109.27
 *
 *)
