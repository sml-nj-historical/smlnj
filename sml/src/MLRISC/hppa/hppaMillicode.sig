signature HPPA_MILLICODE = sig
  structure I : HPPAINSTR

  val divu : {rs:int, rt:int, rd:int} -> I.instruction list
  val mulo : {rs:int, rt:int, rd:int} -> I.instruction list
  val divo : {rs:int, rt:int, rd:int} -> I.instruction list
  val mulu : {rs:int, rt:int, rd:int} -> I.instruction list
  val cvti2d : {rs:int, fd:int} -> I.instruction list
end

(*
 * $Log: hppaMillicode.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
