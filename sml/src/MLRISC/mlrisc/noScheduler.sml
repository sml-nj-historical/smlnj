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
 * $Log$
 *)
