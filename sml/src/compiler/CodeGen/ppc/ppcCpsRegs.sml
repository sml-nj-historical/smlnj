(* ppcCpsRegs.sml --- CPS registers used on the POWER PC
 *
 * COPYRIGHT (c) 1999  Bell Laboratories.
 *
 *)

structure PPCCpsRegs : CPSREGS = 
struct
  structure T = PPCMLTree
  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto

  val GP = PPCCells.GPReg
  val FP = PPCCells.FPReg
  val CC = PPCCells.Reg PPCCells.CC

  fun REG r = T.REG(32, GP r) 
  fun FREG f = T.FREG(64, FP f)

  val exhaustedR = CC 0
  val exhausted	= SOME(T.CC(T.GTU,exhaustedR)) 

  val allocptr 	= REG(14) 
  val limitptr 	= REG(15)
  val storeptr	= REG(16)
  val stdlink	= REG(17)
  val stdclos	= REG(18)
  val stdarg	= REG(19)
  val stdcont  	= REG(20)
  val exnptr	= REG(21)
  val varptr	= REG(22)
  val baseptr   = REG(23)
  val stackptr	= REG(1)
  val gcLink	= T.REG(32,PPCCells.lr) 

  val miscregs =  map REG ([24,25,26,27,29,30,31] @ (3 upto 13)) 
  val calleesave = Array.fromList(miscregs)
  val floatregs = map FREG (1 upto 31)
  val savedfpregs = []

  val availR = 
    map (fn T.REG(_,r) => r)
         ([stdlink, stdclos, stdarg, stdcont] @ miscregs)

  local
      structure ILS = IntListSet
      fun l2s l = ILS.addList (ILS.empty, l)
      val s2l = ILS.listItems
      val -- = ILS.difference
      infix --
  in
      val allRegs = l2s (map GP (0 upto 31))
      val dedicatedR = s2l (allRegs -- l2s availR)
  end

  val availF = map FP (1 upto 31)
  val dedicatedF = [FP 0]

  val signedGCTest = false
  val addressWidth = 32
end
