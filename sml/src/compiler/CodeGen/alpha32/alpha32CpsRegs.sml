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

  val stdarg	= T.REG 0
  val stdcont  	= T.REG 1
  val stdclos	= T.REG 2
  val stdlink	= T.REG 3
  val baseptr   = T.REG 4

  val limitptr 	= T.REG 9
  val varptr	= T.REG 10
  val exhaustedR = 11
  val exhausted	= SOME(T.CC  exhaustedR)
  val storeptr	= T.REG 12
  val allocptr 	= T.REG 13
  val exnptr	= T.REG 14

  val gcLink	= T.REG 26
  val stackptr	= T.REG 30

  val miscregs =  map T.REG ((5 upto 8) @ (15 upto 25) @ [27])
  val calleesave = Array.fromList(miscregs)
  val floatregs = map T.FREG (0 upto 28)
  val savedfpregs = []

  val allRegs = SL.uniq(0 upto 31)

  val availR = 
    map (fn T.REG r => r)
         ([gcLink, T.REG exhaustedR, 
	   stdlink, stdclos, stdarg, stdcont] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = 0 upto 28
  val dedicatedF = [29, 30, 31]
end

(*
 * $Log: alpha32CpsRegs.sml,v $
 * Revision 1.3  1998/05/23 14:09:12  george
 *   Fixed RCS keyword syntax
 *
 *)
