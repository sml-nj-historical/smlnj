(* alpha32CpsRegs.sml --- CPS registers used on the DEC Alpha
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure Alpha32CpsRegs : CPSREGS = 
struct
  structure T = Alpha32MLTree
  structure C = AlphaCells

  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto

  val GP = AlphaCells.GPReg
  val FP = AlphaCells.FPReg

  fun REG r = T.REG(32, GP r)
  fun FREG f = T.FREG(64, FP f) 
  val vfp		= AlphaCells.newReg()
  val vfptr		= T.REG(32, vfp)
  fun stdarg _		= REG(0)
  fun stdcont _		= REG(1)
  fun stdclos _		= REG(2)
  fun stdlink _		= REG(3)
  fun baseptr _		= REG(4)

  fun limitptr _	= REG(9)
  fun varptr _		= REG(10)
  val exhaustedR	= GP 11
  val exhausted		= SOME(T.CC(T.GT,exhaustedR)) 
  fun storeptr _	= REG(12) 
  fun exnptr _		= REG(14)
  fun gcLink _		= REG(26)

  val allocptr		= REG(13)
  val stackptr		= REG(30)

  val miscregs =  map REG ((5 upto 8) @ (15 upto 25) @ [27])
  val calleesave = Array.fromList(miscregs)
  val floatregs = map FREG (0 upto 28)
  val savedfpregs = []

  val availR = 
      map (fn T.REG(_,r) => r)
          ([gcLink(false), T.REG(32, exhaustedR), 
	    stdlink(false), stdclos(false), stdarg(false), stdcont(false)] @ miscregs)

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

