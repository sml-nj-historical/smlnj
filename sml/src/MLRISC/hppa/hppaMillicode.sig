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
 * Revision 1.2  1997/08/29 11:01:32  george
 *   Added mulu and cvti2d
 *
# Revision 1.1.1.1  1997/04/19  18:14:23  george
#   Version 109.27
#
 *)
