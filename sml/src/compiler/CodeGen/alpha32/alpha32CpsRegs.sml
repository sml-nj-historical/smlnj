(* alpha32CpsRegs.sml --- CPS registers used on the DEC Alpha
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure Alpha32CpsRegs : CPSREGS = 
struct
  structure T = Alpha32MLTree
  structure SL = SortedList
  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto

  val GP = AlphaCells.GPReg
  val FP = AlphaCells.FPReg

  val stdarg	= T.REG(32, GP 0)
  val stdcont  	= T.REG(32, GP 1)
  val stdclos	= T.REG(32, GP 2)
  val stdlink	= T.REG(32, GP 3)
  val baseptr   = T.REG(32, GP 4)

  val limitptr 	= T.REG(32, GP 9)
  val varptr	= T.REG(32, GP 10)
  val exhaustedR = GP 11
  val exhausted	= SOME(T.CC(exhaustedR))
  val storeptr	= T.REG(32, GP 12)
  val allocptr 	= T.REG(32, GP 13)
  val exnptr	= T.REG(32, GP 14)

  val gcLink	= T.REG(32, GP 26)
  val stackptr	= T.REG(32, GP 30)

  val miscregs =  map (fn r => T.REG(32,GP r)) 
          ((5 upto 8) @ (15 upto 25) @ [27])
  val calleesave = Array.fromList(miscregs)
  val floatregs = map (fn f => T.FREG(64,FP f)) (0 upto 28)
  val savedfpregs = []

  val allRegs = SL.uniq(GP 0 upto GP 31)

  val availR = 
    map (fn T.REG(_,r) => r)
         ([gcLink, T.REG(32, GP exhaustedR), 
	   stdlink, stdclos, stdarg, stdcont] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = FP 0 upto FP 28
  val dedicatedF = map FP [29, 30, 31]
  val signedGCTest = true
end

(*
 * $Log: alpha32CpsRegs.sml,v $
 * Revision 1.3  1998/05/23 14:09:12  george
 *   Fixed RCS keyword syntax
 *
 *)
