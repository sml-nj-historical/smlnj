(* alpha32CpsRegs.sml --- CPS registers used on the DEC Alpha
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure PPCCpsRegs : CPSREGS = 
struct
  structure T = PPCMLTree
  structure SL = SortedList
  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto

  val exhaustedR = 0
  val exhausted	= SOME(T.CC 0)

  val allocptr 	= T.REG 14
  val limitptr 	= T.REG 15
  val storeptr	= T.REG 16
  val stdlink	= T.REG 17
  val stdclos	= T.REG 18
  val stdarg	= T.REG 19
  val stdcont  	= T.REG 20
  val exnptr	= T.REG 21
  val varptr	= T.REG 22
  val baseptr   = T.REG 23
  val maskreg	= T.REG 29

  val stackptr	= T.REG 1

  val miscregs =  map T.REG ([24,25,26,27] @ (3 upto 13))
  val calleesave = Array.fromList(miscregs)
  val floatregs = map T.FREG (1 upto 31)
  val savedfpregs = []

  val allRegs = SL.uniq(0 upto 31)

  val availR = 
    map (fn T.REG r => r)
         ([maskreg, stdlink, stdclos, stdarg, stdcont] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = 1 upto 31
  val dedicatedF = [0]
end
