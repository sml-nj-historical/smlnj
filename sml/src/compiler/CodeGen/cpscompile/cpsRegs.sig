(* cpsRegs.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The registers used for CPS compilation.
 *
 *)

signature CPSREGS = sig
  structure T : MLTREE
  type rexp = (unit, unit, unit, unit) T.rexp
  type fexp = (unit, unit, unit, unit) T.fexp
  type ccexp = (unit, unit, unit, unit) T.ccexp

  val allocptr 	: rexp	(* must be a regisiter, - T.REG(r) *)
  val limitptr 	: rexp
  val stdlink	: rexp
  val stdclos	: rexp
  val stdarg 	: rexp 
  val stdcont 	: rexp 
  val exnptr 	: rexp 
  val varptr  	: rexp 
  val baseptr	: rexp
  val storeptr 	: rexp 
  val stackptr 	: rexp 
  val gcLink	: rexp 
  
  val calleesave: rexp Array.array
  val exhausted : ccexp option
  val signedGCTest : bool
  val addressWidth : int

  val miscregs  : rexp list
  val floatregs : fexp list
  val savedfpregs : fexp list

  val dedicatedR : int list
  val availR     : int list
  val dedicatedF : int list
  val availF     : int list
end
