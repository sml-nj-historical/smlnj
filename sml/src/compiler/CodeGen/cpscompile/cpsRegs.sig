(* cpsRegs.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The registers used for CPS compilation.
 *
 *)

signature CPSREGS = sig
  structure T : MLTREE

  val allocptr 	: T.rexp	(* must be a register, - T.REG(r) *)
  val limitptr 	: T.rexp
  val stdlink	: T.rexp
  val stdclos	: T.rexp
  val stdarg 	: T.rexp 
  val stdcont 	: T.rexp 
  val exnptr 	: T.rexp 
  val varptr  	: T.rexp 
  val baseptr	: T.rexp
  val storeptr 	: T.rexp 
  val stackptr 	: T.rexp 
  val gcLink	: T.rexp 
  
  val calleesave: T.rexp Array.array
  val exhausted : T.ccexp option
  val signedGCTest : bool
  val addressWidth : T.ty

  val miscregs  : T.rexp list
  val floatregs : T.fexp list
  val savedfpregs : T.fexp list

  val dedicatedR : T.reg list
  val availR     : T.reg list
  val dedicatedF : T.reg list
  val availF     : T.reg list
end
