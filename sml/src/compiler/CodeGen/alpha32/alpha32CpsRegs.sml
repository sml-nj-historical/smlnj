(* alpha32CpsRegs.sml --- CPS registers used on the DEC Alpha
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure Alpha32CpsRegs : CPSREGS = 
struct
  structure T = Alpha32MLTree

  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto

  val GP = AlphaCells.GPReg
  val FP = AlphaCells.FPReg

  fun REG r = T.REG(32, GP r)
  fun FREG f = T.FREG(64, FP f) 

  val stdarg	= REG(0) 
  val stdcont  	= REG(1)
  val stdclos	= REG(2)
  val stdlink	= REG(3)
  val baseptr   = REG(4)

  val limitptr 	= REG(9)
  val varptr	= REG(10)
  val exhaustedR = GP 11
  val exhausted	= SOME(T.CC(T.GT,exhaustedR)) 
  val storeptr	= REG(12) 
  val allocptr 	= REG(13)
  val exnptr	= REG(14)

  val gcLink	= REG(26)
  val stackptr	= REG(30)

  val miscregs =  map REG ((5 upto 8) @ (15 upto 25) @ [27])
  val calleesave = Array.fromList(miscregs)
  val floatregs = map FREG (0 upto 28)
  val savedfpregs = []

  val availR = 
      map (fn T.REG(_,r) => r)
          ([gcLink, T.REG(32, exhaustedR), 
	    stdlink, stdclos, stdarg, stdcont] @ miscregs)

  local
      structure SC = AlphaCells.SortedCells
      val -- = SC.difference
      infix --
  in
      val allRegs = map GP (0 upto 31)
      val dedicatedR = SC.return (SC.uniq allRegs -- SC.uniq availR)
  end

  val availF = map FP (0 upto 28)
  val dedicatedF = map FP [29, 30, 31]
  val signedGCTest = true
  val addressWidth = 64
end

