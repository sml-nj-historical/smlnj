(* ppcCpsRegs.sml --- CPS registers used on the POWER PC
 *
 * COPYRIGHT (c) 1999  Bell Laboratories.
 *
 *)

structure PPCCpsRegs : CPSREGS = 
struct
  structure T = PPCMLTree
  structure SL = SortedList
  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto

  val GP = PPCCells.GPReg
  val FP = PPCCells.FPReg
  val CC = PPCCells.Reg PPCCells.CC

  val exhaustedR = CC 0
  val exhausted	= SOME(T.CC(exhaustedR))

  val allocptr 	= T.REG(32,GP 14)
  val limitptr 	= T.REG(32,GP 15)
  val storeptr	= T.REG(32,GP 16)
  val stdlink	= T.REG(32,GP 17)
  val stdclos	= T.REG(32,GP 18)
  val stdarg	= T.REG(32,GP 19)
  val stdcont  	= T.REG(32,GP 20)
  val exnptr	= T.REG(32,GP 21)
  val varptr	= T.REG(32,GP 22)
  val baseptr   = T.REG(32,GP 23)
  val stackptr	= T.REG(32,GP 1)
  val gcLink	= T.REG(32,PPCCells.lr)

  val miscregs =  map (fn r => T.REG(32,GP r)) 
                     ([24,25,26,27,29,30,31] @ (3 upto 13))
  val calleesave = Array.fromList(miscregs)
  val floatregs = map (fn f => T.FREG(64,FP f)) (1 upto 31)
  val savedfpregs = []

  val allRegs = map GP (SL.uniq(0 upto 31))

  val availR = 
    map (fn T.REG(_,r) => r)
         ([stdlink, stdclos, stdarg, stdcont] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = map FP (1 upto 31)
  val dedicatedF = [FP 0]

  val signedGCTest = false
end
